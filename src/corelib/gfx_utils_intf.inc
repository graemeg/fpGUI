{%mainunit gfx_utils.pas}

// Common functions for all platforms
function fpgAddTrailingValue(const ALine, AValue: string; ADuplicates: boolean = true): string;



// Functions with platform specific implementations

// File utils
function ExtractTargetSymLinkPath(ALink: string): string;
function FileIsSymlink(const AFilename: string): boolean;



