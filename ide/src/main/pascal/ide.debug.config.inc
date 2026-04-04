{ ide.debug.config.inc
  Debugger feature availability guard.

  OPDF-based debugging requires FPC 3.3.x (trunk) which generates OPDF
  debug information. When building with FPC 3.2.x the debugger is compiled
  out and replaced by a no-op stub so the rest of the IDE still works.
}
{$IF FPC_FULLVERSION >= 30300}
  {$DEFINE HAS_OPDF_DEBUG}
{$ENDIF}
