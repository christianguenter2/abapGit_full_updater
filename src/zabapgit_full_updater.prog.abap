*&---------------------------------------------------------------------*
*& Report zabapgit_full_updater
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapgit_full_updater.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  text TYPE abap_bool RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND t,
  url  TYPE string LOWER CASE DEFAULT 'https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap'.

SELECTION-SCREEN SKIP.

PARAMETERS:
  artifact TYPE abap_bool RADIOBUTTON GROUP r1,
  art_id   TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP.

PARAMETERS:
  report   TYPE programm OBLIGATORY DEFAULT 'ZABAPGIT_FULL'.

CLASS controller DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      run,
      at_selection_screen_output,
      on_f4_artifact_id.

  PRIVATE SECTION.
    CLASS-DATA:
      source_tab TYPE abaptxt255_tab.

    CLASS-METHODS:
      check_report
        RAISING
          zcx_abapgit_exception,

      fetch_from_url
        IMPORTING
          i_url         TYPE string OPTIONAL
        RETURNING
          VALUE(result) TYPE xstring
        RAISING
          zcx_abapgit_exception,

      update_report
        RAISING
          zcx_abapgit_exception,

      get_source_from_zip
        IMPORTING
          i_zip         TYPE xstring
        RETURNING
          VALUE(result) TYPE xstring
        RAISING
          zcx_abapgit_exception,

      fetch_from_gh_actions_artifact
        RETURNING
          VALUE(result) TYPE string
        RAISING
          zcx_abapgit_exception,

      fetch_from_url_auth
        IMPORTING
          i_url         TYPE string
        RETURNING
          VALUE(result) TYPE xstring
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

CLASS controller IMPLEMENTATION.

  METHOD run.

    DATA: response TYPE xstring.

    TRY.
        check_report( ).

        CASE abap_true.
          WHEN text.

            response = fetch_from_url( url ).

          WHEN artifact.

            response = fetch_from_gh_actions_artifact( ).

        ENDCASE.

        DATA(report_source) = zcl_abapgit_convert=>xstring_to_string_utf8( response ).

        SPLIT report_source AT cl_abap_char_utilities=>newline
          INTO TABLE source_tab.

        update_report( ).

      CATCH zcx_abapgit_exception INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD check_report.

    IF CAST zif_abapgit_object( NEW zcl_abapgit_object_prog(
                        is_item     = VALUE #( obj_name = report )
                        iv_language = sy-langu ) )->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Report { report } doesn't exist| ).
    ENDIF.

  ENDMETHOD.


  METHOD fetch_from_url.

    DATA:
      http_client TYPE REF TO if_http_client.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = i_url
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    http_client->request->set_method( if_http_entity=>co_request_method_get ).

    http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    http_client->get_last_error(
      IMPORTING
        code    = DATA(error_code)
        message = DATA(message) ).

    IF error_code IS NOT INITIAL.
      zcx_abapgit_exception=>raise( |HTTP error { error_code } occured. Message: { message }| ).
    ENDIF.

    http_client->response->get_status(
      IMPORTING
        code   = DATA(code)
        reason = DATA(reason) ).

    IF code <> 200.
      zcx_abapgit_exception=>raise( |HTTP status code { code }. Message: { reason }| ).
    ENDIF.

    result = http_client->response->get_data( ).

  ENDMETHOD.


  METHOD update_report.

    DATA:
      item    TYPE zif_abapgit_definitions=>ty_item,
      progdir TYPE progdir.

    ASSERT lines( source_tab ) > 0.

    SELECT
      SINGLE FROM tadir
      FIELDS
        object AS obj_type,
        obj_name,
        devclass
      WHERE pgmid    = 'R3TR'
      AND   object   = 'PROG'
      AND   obj_name = @report
      INTO CORRESPONDING FIELDS OF @item.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = report
      IMPORTING
        e_progdir  = progdir
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    NEW zcl_abapgit_objects_program(
      is_item     = item
      iv_language = sy-langu
    )->deserialize_program(
      is_progdir = progdir
      it_source  = source_tab
      it_tpool   = VALUE #(  )
      iv_package = item-devclass ).

    DATA(log) = CAST zif_abapgit_log( NEW zcl_abapgit_log( ) ).

    zcl_abapgit_objects_activation=>activate( log ).
    IF log->count( ) > 0.
      zcl_abapgit_log_viewer=>show_log( log ).
    ENDIF.

    MESSAGE |Report { report } updated| TYPE 'S'.

  ENDMETHOD.


  METHOD get_source_from_zip.

    DATA(zip) = NEW cl_abap_zip( ).

    zip->load(
      EXPORTING
        zip             = i_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    zip->get(
      EXPORTING
        index   = 1
      IMPORTING
        content = result
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |ZIP error: { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD at_selection_screen_output.

    LOOP AT SCREEN.

      CASE abap_true.
        WHEN text.

          IF screen-name = 'ART_ID'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.

        WHEN artifact.

          IF screen-name = 'URL'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD fetch_from_gh_actions_artifact.

    " https://docs.github.com/en/rest/actions/artifacts#get-an-artifact
    " https://docs.github.com/en/rest/actions/artifacts#download-an-artifact

    " GET https://api.github.com/repos/OWNER/REPO/actions/artifacts/ARTIFACT_ID

    DATA(url) = |https://api.github.com/repos/abapGit/abapGit/actions/artifacts/{ art_id }|.

    DATA(json) = zcl_abapgit_convert=>xstring_to_string_utf8( fetch_from_url_auth( url ) ).

    TRY.
        DATA(zip_url) = zcl_ajson=>parse( json )->get( '/archive_download_url' ).

      CATCH zcx_ajson_error INTO DATA(error).
        zcx_abapgit_exception=>raise_with_text( error ).
    ENDTRY.

    result = get_source_from_zip( fetch_from_url_auth( zip_url ) ).

  ENDMETHOD.


  METHOD fetch_from_url_auth.

    DATA(http_client) = zcl_abapgit_exit=>get_instance( )->create_http_client( i_url ).

    http_client->request->set_header_field(
        name  = '~request_uri'
        value = zcl_abapgit_url=>path( i_url ) && zcl_abapgit_url=>name( i_url ) ).
    http_client->request->set_method( if_http_entity=>co_request_method_get ).

    http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    http_client->get_last_error(
      IMPORTING
        code    = DATA(error_code)
        message = DATA(message) ).

    IF error_code IS NOT INITIAL.
      zcx_abapgit_exception=>raise( |HTTP error { error_code } occured. Message: { message }| ).
    ENDIF.

    http_client->response->get_status(
      IMPORTING
        code   = DATA(code)
        reason = DATA(reason) ).

    IF code <> 200.
      zcx_abapgit_exception=>raise( |HTTP status code { code }. Message: { reason }| ).
    ENDIF.

    result = http_client->response->get_data( ).

  ENDMETHOD.


  METHOD on_f4_artifact_id.

    TYPES:
      BEGIN OF t_artifact,
        id         TYPE string,
        created_at TYPE string,
        branch     TYPE string,
      END OF t_artifact,
      tt_artifact TYPE STANDARD TABLE OF t_artifact
                       WITH NON-UNIQUE DEFAULT KEY.

    DATA:
      artifacts TYPE tt_artifact,
      artifact  LIKE LINE OF artifacts,
      selected  LIKE artifacts.


    DATA(url) = |https://api.github.com/repos/abapGit/abapGit/actions/artifacts|.

    TRY.
        DATA(json) = zcl_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( fetch_from_url_auth( url ) ) ).

        DATA(json_artifacts) = json->members( '/artifacts' ).

        LOOP AT json_artifacts ASSIGNING FIELD-SYMBOL(<artifact>).

          CLEAR: artifact.

          IF json->get( |/artifacts/{ <artifact> }/name| ) NS 'zabapgit_standalone'.
            CONTINUE.
          ENDIF.

          artifact-id = json->get( |/artifacts/{ <artifact> }/id| ).
          artifact-created_at = json->get( |/artifacts/{ <artifact> }/created_at| ).
          artifact-branch = json->get( |/artifacts/{ <artifact> }/workflow_run/head_branch| ).

          INSERT artifact INTO TABLE artifacts.

        ENDLOOP.

        SORT artifacts BY branch created_at DESCENDING.
        DELETE ADJACENT DUPLICATES FROM artifacts COMPARING branch.

        zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_from_list(
          EXPORTING
            it_list               = artifacts
            iv_selection_mode     = if_salv_c_selection_mode=>single
            it_columns_to_display = VALUE #( ( name = |ID| ) ( name = |CREATED_AT| ) ( name = |BRANCH| ) )
          IMPORTING
            et_list               = selected ).

        art_id = VALUE #( selected[ 1 ]-id DEFAULT art_id ).

      CATCH zcx_ajson_error zcx_abapgit_exception INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR art_id.
  controller=>on_f4_artifact_id( ).

AT SELECTION-SCREEN OUTPUT.
  controller=>at_selection_screen_output( ).

START-OF-SELECTION.
  controller=>run( ).
