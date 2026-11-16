{ ide.debug.config.inc
  Debugger feature availability guard.

  OPDF-based debugging is opt-in. It requires BOTH:
    - the OPDF_DEBUG define (PasBuild profile 'opdf'), which also needs the
      opdf-lib / pdr-* dependencies enabled in ide/project.xml, and
    - an FPC that generates OPDF debug information (3.3.x or later with
      OPDF support).

  Otherwise the debugger is compiled out and replaced by a no-op stub so
  the rest of the IDE still works. No released FPC (3.2.2, 3.3.1) supports
  OPDF yet, so the stub is the default.
}
{$IFDEF OPDF_DEBUG}
  {$IF FPC_FULLVERSION >= 30300}
    {$DEFINE HAS_OPDF_DEBUG}
  {$ENDIF}
{$ENDIF}
