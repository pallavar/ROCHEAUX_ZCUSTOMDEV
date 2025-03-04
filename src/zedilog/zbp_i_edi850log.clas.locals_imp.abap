CLASS lhc_zedi850_log DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zedi850_log RESULT result.

ENDCLASS.

CLASS lhc_zedi850_log IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.



ENDCLASS.
