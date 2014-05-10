unit u_data;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

const
  cp874_n: array[0..255] of string =
    ('.notdef',           '.notdef',          '.notdef',        '.notdef',            // 00 to 03
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 04 to 07
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 10 to 13
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 14 to 17
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 1C to 1F
     'space',             'exclam',           'quotedbl',       'numbersign',         // 20 to 23
     'dollar',            'percent',          'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',         'parenright',       'asterisk',       'plus',               // 28 to 2B
     'comma',             'hyphen',           'period',         'slash',              // 2C to 2F
     'zero',              'one',              'two',            'three',              // 30 to 33
     'four',              'five',             'six',            'seven',              // 34 to 37
     'eight',             'nine',             'colon',          'semicolon',          // 38 to 3B
     'less',              'equal',            'greater',        'question',           // 3C to 3F
     'at',                'A',                'B',              'C',                  // 40 to 43
     'D',                 'E',                'F',              'G',                  // 44 to 47
     'H',                 'I',                'J',              'K',                  // 48 to 4B
     'L',                 'M',                'N',              'O',                  // 4C to 4F
     'P',                 'Q',                'R',              'S',                  // 50 to 53
     'T',                 'U',                'V',              'W',                  // 54 to 57
     'X',                 'Y',                'Z',              'bracketleft',        // 58 to 5B
     'backslash',         'bracketright',     'asciicircum',    'underscore',         // 5C to 5F
     'grave',             'a',                'b',              'c',                  // 60 to 63
     'd',                 'e',                'f',              'g',                  // 64 to 67
     'h',                 'i',                'j',              'k',                  // 68 to 6B
     'l',                 'm',                'n',              'o',                  // 6C to 6F
     'p',                 'q',                'r',              's',                  // 70 to 73
     't',                 'u',                'v',              'w',                  // 74 to 77
     'x',                 'y',                'z',              'braceleft',          // 78 to 7B
     'bar',               'braceright',       'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',              '.notdef',          '.notdef',        '.notdef',            // 80 to 83
     '.notdef',           'ellipsis',         '.notdef',        '.notdef',            // 84 to 87
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 88 to 8B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 8C to 8F
     '.notdef',           'quoteleft',        'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright',     'bullet',           'endash',         'emdash',             // 94 to 97
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 98 to 9B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 9C to 9F
     'space',             'kokaithai',        'khokhaithai',    'khokhuatthai',       // A0 to A3
     'khokhwaithai',      'khokhonthai',      'khorakhangthai', 'ngonguthai',         // A4 to A7
     'chochanthai',       'chochingthai',     'chochangthai',   'sosothai',           // A8 to AB
     'chochoethai',       'yoyingthai',       'dochadathai',    'topatakthai',        // AC to AF
     'thothanthai',       'thonangmonthothai','thophuthaothai', 'nonenthai',          // B0 to B3
     'dodekthai',         'totaothai',        'thothungthai',   'thothahanthai',      // B4 to B7
     'thothongthai',      'nonuthai',         'bobaimaithai',   'poplathai',          // B8 to BB
     'phophungthai',      'fofathai',         'phophanthai',    'fofanthai',          // BC to BF
     'phosamphaothai',    'momathai',         'yoyakthai',      'roruathai',          // C0 to C3
     'ruthai',            'lolingthai',       'luthai',         'wowaenthai',         // C4 to C7
     'sosalathai',        'sorusithai',       'sosuathai',      'hohipthai',          // C8 to CB
     'lochulathai',       'oangthai',         'honokhukthai',   'paiyannoithai',      // CC to CF
     'saraathai',         'maihanakatthai',   'saraaathai',     'saraamthai',         // D0 to D3
     'saraithai',         'saraiithai',       'sarauethai',     'saraueethai',        // D4 to D7
     'sarauthai',         'sarauuthai',       'phinthuthai',    '.notdef',            // D8 to DB
     '.notdef',           '.notdef',          '.notdef',        'bahtthai',           // DC to DF
     'saraethai',         'saraaethai',       'saraothai',      'saraaimaimuanthai',  // E0 to E3
     'saraaimaimalaithai','lakkhangyaothai',  'maiyamokthai',   'maitaikhuthai',      // E4 to E7
     'maiekthai',         'maithothai',       'maitrithai',     'maichattawathai',    // E8 to EB
     'thanthakhatthai',   'nikhahitthai',     'yamakkanthai',   'fongmanthai',        // EC to EF
     'zerothai',          'onethai',          'twothai',        'threethai',          // F0 to F3
     'fourthai',          'fivethai',         'sixthai',        'seventhai',          // F4 to F7
     'eightthai',         'ninethai',         'angkhankhuthai', 'khomutthai',         // F8 to FB
     '.notdef',           '.notdef',          '.notdef',        '.notdef');           // FC to FF

const
  cp874_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8364,    -1,      -1,      -1,      -1,       8230,    -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     3585,    3586,    3587,    3588,     3589,    3590,    3591,      // A0 to A7
     3592,    3593,    3594,    3595,    3596,     3597,    3598,    3599,      // A8 to AF
     3600,    3601,    3602,    3603,    3604,     3605,    3606,    3607,      // B0 to B7
     3608,    3609,    3610,    3611,    3612,     3613,    3614,    3615,      // B8 to BF
     3616,    3617,    3618,    3619,    3620,     3621,    3622,    3623,      // C0 to C7
     3624,    3625,    3626,    3627,    3628,     3629,    3630,    3631,      // C8 to CF
     3632,    3633,    3634,    3635,    3636,     3637,    3638,    3639,      // D0 to D7
     3640,    3641,    3642,    -1,      -1,       -1,      -1,      3647,      // D8 to DF
     3648,    3649,    3650,    3651,    3652,     3653,    3654,    3655,      // E0 to E7
     3656,    3657,    3658,    3659,    3660,     3661,    3662,    3663,      // E8 to EF
     3664,    3665,    3666,    3667,    3668,     3669,    3670,    3671,      // F0 to F7
     3672,    3673,    3674,    3675,    -1,       -1,      -1,      -1);       // F8 to FF

const
  cp1250_n: array[0..255] of string =
    ('.notdef',       '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',         'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',        'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',     'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',         'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',          'one',          'two',            'three',              // 30 to 33
     'four',          'five',         'six',            'seven',              // 34 to 37
     'eight',         'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',          'equal',        'greater',        'question',           // 3C to 3F
     'at',            'A',            'B',              'C',                  // 40 to 43
     'D',             'E',            'F',              'G',                  // 44 to 47
     'H',             'I',            'J',              'K',                  // 48 to 4B
     'L',             'M',            'N',              'O',                  // 4C to 4F
     'P',             'Q',            'R',              'S',                  // 50 to 53
     'T',             'U',            'V',              'W',                  // 54 to 57
     'X',             'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',     'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',         'a',            'b',              'c',                  // 60 to 63
     'd',             'e',            'f',              'g',                  // 64 to 67
     'h',             'i',            'j',              'k',                  // 68 to 6B
     'l',             'm',            'n',              'o',                  // 6C to 6F
     'p',             'q',            'r',              's',                  // 70 to 73
     't',             'u',            'v',              'w',                  // 74 to 77
     'x',             'y',            'z',              'braceleft',          // 78 to 7B
     'bar',           'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',          '.notdef',      'quotesinglbase', '.notdef',            // 80 to 83
     'quotedblbase',  'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     '.notdef',       'perthousand',  'Scaron',         'guilsinglleft',      // 88 to 8B
     'Sacute',        'Tcaron',       'Zcaron',         'Zacute',             // 8C to 8F
     '.notdef',       'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright', 'bullet',       'endash',         'emdash',             // 94 to 97
     '.notdef',       'trademark',    'scaron',         'guilsinglright',     // 98 to 9B
     'sacute',        'tcaron',       'zcaron',         'zacute',             // 9C to 9F
     'space',         'caron',        'breve',          'Lslash',             // A0 to A3
     'currency',      'Aogonek',      'brokenbar',      'section',            // A4 to A7
     'dieresis',      'copyright',    'Scedilla',       'guillemotleft',      // A8 to AB
     'logicalnot',    'hyphen',       'registered',     'Zdotaccent',         // AC to AF
     'degree',        'plusminus',    'ogonek',         'lslash',             // B0 to B3
     'acute',         'mu',           'paragraph',      'periodcentered',     // B4 to B7
     'cedilla',       'aogonek',      'scedilla',       'guillemotright',     // B8 to BB
     'Lcaron',        'hungarumlaut', 'lcaron',         'zdotaccent',         // BC to BF
     'Racute',        'Aacute',       'Acircumflex',    'Abreve',             // C0 to C3
     'Adieresis',     'Lacute',       'Cacute',         'Ccedilla',           // C4 to C7
     'Ccaron',        'Eacute',       'Eogonek',        'Edieresis',          // C8 to CB
     'Ecaron',        'Iacute',       'Icircumflex',    'Dcaron',             // CC to CF
     'Dcroat',        'Nacute',       'Ncaron',         'Oacute',             // D0 to D3
     'Ocircumflex',   'Ohungarumlaut','Odieresis',      'multiply',           // D4 to D7
     'Rcaron',        'Uring',        'Uacute',         'Uhungarumlaut',      // D8 to DB
     'Udieresis',     'Yacute',       'Tcommaaccent',   'germandbls',         // DC to DF
     'racute',        'aacute',       'acircumflex',    'abreve',             // E0 to E3
     'adieresis',     'lacute',       'cacute',         'ccedilla',           // E4 to E7
     'ccaron',        'eacute',       'eogonek',        'edieresis',          // E8 to EB
     'ecaron',        'iacute',       'icircumflex',    'dcaron',             // EC to EF
     'dcroat',        'nacute',       'ncaron',         'oacute',             // F0 to F3
     'ocircumflex',   'ohungarumlaut','odieresis',      'divide',             // F4 to F7
     'rcaron',        'uring',        'uacute',         'uhungarumlaut',      // F8 to FB
     'udieresis',     'yacute',       'tcommaaccent',   'dotaccent');         // FC to FF

const
  cp1250_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8364,    -1,      8218,    -1,      8222,     8230,    8224,    8225,      // 80 to 87
     -1,      8240,    352,     8249,    346,      356,     381,     377,       // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     -1,      8482,    353,     8250,    347,      357,     382,     378,       // 98 to 9F
     160,     711,     728,     321,     164,      260,     166,     167,       // A0 to A7
     168,     169,     350,     171,     172,      173,     174,     379,       // A8 to AF
     176,     177,     731,     322,     180,      181,     182,     183,       // B0 to B7
     184,     261,     351,     187,     317,      733,     318,     380,       // B8 to BF
     340,     193,     194,     258,     196,      313,     262,     199,       // C0 to C7
     268,     201,     280,     203,     282,      205,     206,     270,       // C8 to CF
     272,     323,     327,     211,     212,      336,     214,     215,       // D0 to D7
     344,     366,     218,     368,     220,      221,     354,     354,       // D8 to DF
     341,     225,     226,     259,     228,      314,     263,     231,       // E0 to E7
     269,     233,     281,     235,     283,      237,     238,     271,       // E8 to EF
     273,     324,     328,     243,     244,      337,     246,     247,       // F0 to F7
     345,     367,     250,     369,     252,      253,     355,     729);      // F8 to FF

const
  cp1251_n: array[0..255] of string =
    ('.notdef',       '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',         'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',        'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',     'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',         'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',          'one',          'two',            'three',              // 30 to 33
     'four',          'five',         'six',            'seven',              // 34 to 37
     'eight',         'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',          'equal',        'greater',        'question',           // 3C to 3F
     'at',            'A',            'B',              'C',                  // 40 to 43
     'D',             'E',            'F',              'G',                  // 44 to 47
     'H',             'I',            'J',              'K',                  // 48 to 4B
     'L',             'M',            'N',              'O',                  // 4C to 4F
     'P',             'Q',            'R',              'S',                  // 50 to 53
     'T',             'U',            'V',              'W',                  // 54 to 57
     'X',             'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',     'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',         'a',            'b',              'c',                  // 60 to 63
     'd',             'e',            'f',              'g',                  // 64 to 67
     'h',             'i',            'j',              'k',                  // 68 to 6B
     'l',             'm',            'n',              'o',                  // 6C to 6F
     'p',             'q',            'r',              's',                  // 70 to 73
     't',             'u',            'v',              'w',                  // 74 to 77
     'x',             'y',            'z',              'braceleft',          // 78 to 7B
     'bar',           'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'afii10051',     'afii10052',    'quotesinglbase', 'afii10100',          // 80 to 83
     'quotedblbase',  'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     'Euro',          'perthousand',  'afii10058',      'guilsinglleft',      // 88 to 8B
     'afii10059',     'afii10061',    'afii10060',      'afii10145',          // 8C to 8F
     'afii10099',     'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright', 'bullet',       'endash',         'emdash',             // 94 to 97
     '.notdef',       'trademark',    'afii10106',      'guilsinglright',     // 98 to 9B
     'afii10107',     'afii10109',    'afii10108',      'afii10193',          // 9C to 9F
     'space',         'afii10062',    'afii10110',      'afii10057',          // A0 to A3
     'currency',      'afii10050',    'brokenbar',      'section',            // A4 to A7
     'afii10023',     'copyright',    'afii10053',      'guillemotleft',      // A8 to AB
     'logicalnot',    'hyphen',       'registered',     'afii10056',          // AC to AF
     'degree',        'plusminus',    'afii10055',      'afii10103',          // B0 to B3
     'afii10098',     'mu',           'paragraph',      'periodcentered',     // B4 to B7
     'afii10071',     'afii61352',    'afii10101',      'guillemotright',     // B8 to BB
     'afii10105',     'afii10054',    'afii10102',      'afii10104',          // BC to BF
     'afii10017',     'afii10018',    'afii10019',      'afii10020',          // C0 to C3
     'afii10021',     'afii10022',    'afii10024',      'afii10025',          // C4 to C7
     'afii10026',     'afii10027',    'afii10028',      'afii10029',          // C8 to CB
     'afii10030',     'afii10031',    'afii10032',      'afii10033',          // CC to CF
     'afii10034',     'afii10035',    'afii10036',      'afii10037',          // D0 to D3
     'afii10038',     'afii10039',    'afii10040',      'afii10041',          // D4 to D7
     'afii10042',     'afii10043',    'afii10044',      'afii10045',          // D8 to DB
     'afii10046',     'afii10047',    'afii10048',      'afii10049',          // DC to DF
     'afii10065',     'afii10066',    'afii10067',      'afii10068',          // E0 to E3
     'afii10069',     'afii10070',    'afii10072',      'afii10073',          // E4 to E7
     'afii10074',     'afii10075',    'afii10076',      'afii10077',          // E8 to EB
     'afii10078',     'afii10079',    'afii10080',      'afii10081',          // EC to EF
     'afii10082',     'afii10083',    'afii10084',      'afii10085',          // F0 to F3
     'afii10086',     'afii10087',    'afii10088',      'afii10089',          // F4 to F7
     'afii10090',     'afii10091',    'afii10092',      'afii10093',          // F8 to FB
     'afii10094',     'afii10095',    'afii10095',      'afii10097');         // FC to FF

const
  cp1251_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     1026,    1027,    8218,    1107,    8222,     8230,    8224,    8225,      // 80 to 87
     8364,    8240,    1033,    8249,    1034,     1036,    1035,    1039,      // 88 to 8F
     1106,    8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     -1,      8482,    1113,    8250,    1114,     1116,    1115,    1119,      // 98 to 9F
     160,     1038,    1118,    1032,    164,      1168,    166,     167,       // A0 to A7
     1025,    169,     1028,    171,     172,      173,     174,     1031,      // A8 to AF
     176,     177,     1030,    1110,    1169,     181,     182,     183,       // B0 to B7
     1105,    8470,    1108,    187,     1112,     1029,    1109,    1111,      // B8 to BF
     1040,    1041,    1042,    1043,    1044,     1045,    1046,    1047,      // C0 to C7
     1048,    1049,    1050,    1051,    1052,     1053,    1054,    1055,      // C8 to CF
     1056,    1057,    1058,    1059,    1060,     1061,    1062,    1063,      // D8 to D7
     1064,    1065,    1066,    1067,    1068,     1069,    1070,    1071,      // D8 to DF
     1072,    1073,    1074,    1075,    1076,     1077,    1078,    1079,      // E0 to E7
     1080,    1081,    1082,    1083,    1084,     1085,    1086,    1087,      // E8 to EF
     1088,    1089,    1090,    1091,    1092,     1093,    1094,    1095,      // F0 to F7
     1096,    1097,    1098,    1099,    1100,     1101,    1102,    1103);     // F8 to FF

const
  cp1252_n: array[0..255] of string =
    ('.notdef',       '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',         'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',        'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',     'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',         'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',          'one',          'two',            'three',              // 30 to 33
     'four',          'five',         'six',            'seven',              // 34 to 37
     'eight',         'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',          'equal',        'greater',        'question',           // 3C to 3F
     'at',            'A',            'B',              'C',                  // 40 to 43
     'D',             'E',            'F',              'G',                  // 44 to 47
     'H',             'I',            'J',              'K',                  // 48 to 4B
     'L',             'M',            'N',              'O',                  // 4C to 4F
     'P',             'Q',            'R',              'S',                  // 50 to 53
     'T',             'U',            'V',              'W',                  // 54 to 57
     'X',             'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',     'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',         'a',            'b',              'c',                  // 60 to 63
     'd',             'e',            'f',              'g',                  // 64 to 67
     'h',             'i',            'j',              'k',                  // 68 to 6B
     'l',             'm',            'n',              'o',                  // 6C to 6F
     'p',             'q',            'r',              's',                  // 70 to 73
     't',             'u',            'v',              'w',                  // 74 to 77
     'x',             'y',            'z',              'braceleft',          // 78 to 7B
     'bar',           'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',          '.notdef',      'quotesinglbase', 'florin',             // 80 to 83
     'quotedblbase',  'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     'circumflex',    'perthousand',  'Scaron',         'guilsinglleft',      // 88 to 8B
     'OE',            '.notdef',      'Zcaron',         '.notdef',            // 8C to 8F
     '.notdef',       'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright', 'bullet',       'endash',         'emdash',             // 94 to 97
     'tilde',         'trademark',    'scaron',         'guilsinglright',     // 98 to 9B
     'oe',            '.notdef',      'zcaron',         'Ydieresis',          // 9C to 9F
     'space',         'exclamdown',   'cent',           'sterling',           // A0 to A3
     'currency',      'yen',          'brokenbar',      'section',            // A4 to A7
     'dieresis',      'copyright',    'ordfeminine',    'guillemotleft',      // A8 to AB
     'logicalnot',    'hyphen',       'registered',     'macron',             // AC to AF
     'degree',        'plusminus',    'twosuperior',    'threesuperior',      // B0 to B3
     'acute',         'mu',           'paragraph',      'periodcentered',     // B4 to B7
     'cedilla',       'onesuperior',  'ordmasculine',   'guillemotright',     // B8 to BB
     'onequarter',    'onehalf',      'threequarters',  'questiondown',       // BC to BF
     'Agrave',        'Aacute',       'Acircumflex',    'Atilde',             // C0 to C3
     'Adieresis',     'Aring',        'AE',             'Ccedilla',           // C4 to C7
     'Egrave',        'Eacute',       'Ecircumflex',    'Edieresis',          // C8 to CB
     'Igrave',        'Iacute',       'Icircumflex',    'Idieresis',          // CC to CF
     'Eth',           'Ntilde',       'Ograve',         'Oacute',             // D0 to D3
     'Ocircumflex',   'Otilde',       'Odieresis',      'multiply',           // D4 to D7
     'Oslash',        'Ugrave',       'Uacute',         'Ucircumflex',        // D8 to DB
     'Udieresis',     'Yacute',       'Thorn',          'germandbls',         // DC to DF
     'agrave',        'aacute',       'acircumflex',    'atilde',             // E0 to E3
     'adieresis',     'aring',        'ae',             'ccedilla',           // E4 to E7
     'egrave',        'eacute',       'ecircumflex',    'edieresis',          // E8 to EB
     'igrave',        'iacute',       'icircumflex',    'idieresis',          // EC to EF
     'eth',           'ntilde',       'ograve',         'oacute',             // F0 to F3
     'ocircumflex',   'otilde',       'odieresis',      'divide',             // F4 to F7
     'oslash',        'ugrave',       'uacute',         'ucircumflex',        // F8 to FB
     'udieresis',     'yacute',       'thorn',          'ydieresis');         // FC to FF

const
  cp1252_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8364,    -1,     8218,    402,     8222,     8230,    8224,    8225,      // 80 to 87
     710,     8240,    352,     8249,    338,      -1,      381,     -1,        // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     732,     8482,    353,     8250,    339,      -1,      382,     376,       // 98 to 9F
     160,     161,     162,     163,     164,      165,     166,     167,       // A0 to A7
     168,     169,     170,     171,     172,      173,     174,     175,       // A8 to AF
     176,     177,     178,     179,     180,      181,     182,     183,       // B0 to B7
     184,     185,     186,     187,     188,      189,     190,     191,       // B8 to BF
     192,     193,     194,     195,     196,      197,     198,     199,       // C0 to C7
     200,     201,     202,     203,     204,      205,     206,     207,       // C8 to CF
     208,     209,     210,     211,     212,      213,     214,     215,       // D0 to D7
     216,     217,     218,     219,     220,      221,     222,     223,       // D8 to DF
     224,     225,     226,     227,     228,      229,     230,     231,       // E0 to E7
     232,     233,     234,     235,     236,      237,     238,     239,       // E8 to EF
     240,     241,     242,     243,     244,      245,     246,     247,       // F0 to F7
     248,     249,     250,     251,     252,      253,     254,     255);      // F8 to FF

const
  cp1253_n: array[0..255] of string =
    ('.notdef',             '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',               'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',              'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',           'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',               'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',                'one',          'two',            'three',              // 30 to 33
     'four',                'five',         'six',            'seven',              // 34 to 37
     'eight',               'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',                'equal',        'greater',        'question',           // 3C to 3F
     'at',                  'A',            'B',              'C',                  // 40 to 43
     'D',                   'E',            'F',              'G',                  // 44 to 47
     'H',                   'I',            'J',              'K',                  // 48 to 4B
     'L',                   'M',            'N',              'O',                  // 4C to 4F
     'P',                   'Q',            'R',              'S',                  // 50 to 53
     'T',                   'U',            'V',              'W',                  // 54 to 57
     'X',                   'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',           'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',               'a',            'b',              'c',                  // 60 to 63
     'd',                   'e',            'f',              'g',                  // 64 to 67
     'h',                   'i',            'j',              'k',                  // 68 to 6B
     'l',                   'm',            'n',              'o',                  // 6C to 6F
     'p',                   'q',            'r',              's',                  // 70 to 73
     't',                   'u',            'v',              'w',                  // 74 to 77
     'x',                   'y',            'z',              'braceleft',          // 78 to 7B
     'bar',                 'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',                '.notdef',      'quotesinglbase', 'florin',             // 80 to 83
     'quotedblbase',        'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     '.notdef',             'perthousand',  '.notdef',        'guilsinglleft',      // 88 to 8B
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 8C to 8F
     '.notdef',             'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright',       'bullet',       'endash',         'emdash',             // 94 to 97
     '.notdef',             'trademark',    '.notdef',        'guilsinglright',     // 98 to 9B
     '.notdef',             '.notdef',      '.notdef',        '.notdef',            // 9C to 9F
     'space',               'dieresistonos','Alphatonos',     'sterling',           // A0 to A3
     'currency',            'yen',          'brokenbar',      'section',            // A4 to A7
     'dieresis',            'copyright',    '.notdef',        'guillemotleft',      // A8 to AB
     'logicalnot',          'hyphen',       'registered',     'afii00208',          // AC to AF
     'degree',              'plusminus',    'twosuperior',    'threesuperior',      // B0 to B3
     'tonos',               'mu',           'paragraph',      'periodcentered',     // B4 to B7
     'Epsilontonos',        'Etatonos',     'Iotatonos',      'guillemotright',     // B8 to BB
     'Omicrontonos',        'onehalf',      'Upsilontonos',   'Omegatonos',         // BC to BF
     'iotadieresistonos',   'Alpha',        'Beta',           'Gamma',              // C0 to C3
     'Delta',               'Epsilon',      'Zeta',           'Eta',                // C4 to C7
     'Theta',               'Iota',         'Kappa',          'Lambda',             // C8 to CB
     'Mu',                  'Nu',           'Xi',             'Omicron',            // CC to CF
     'Pi',                  'Rho',          '.notdef',        'Sigma',              // D0 to D3
     'Tau',                 'Upsilon',      'Phi',            'Chi',                // D4 to D7
     'Psi',                 'Omega',        'Iotadieresis',   'Upsilondieresis',    // D8 to DB
     'alphatonos',          'epsilontonos', 'etatonos',       'iotatonos',          // DC to DF
     'upsilondieresistonos','alpha',        'beta',           'gamma',              // E0 to E3
     'delta',               'epsilon',      'zeta',           'eta',                // E4 to E7
     'theta',               'iota',         'kappa',          'lambda',             // E8 to EB
     'mu',                  'nu',           'xi',             'omicron',            // EC to EF
     'pi',                  'rho',          'sigma1',         'sigma',              // F0 to F3
     'tau',                 'upsilon',      'phi',            'chi',                // F4 to F7
     'psi',                 'omega',        'iotadieresis',   'upsilondieresis',    // F8 to FB
     'omicrontonos',        'upsilontonos', 'omegatonos',     '.notdef');           // FC to FF

const
  cp1253_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8364,    -1,      8218,    402,     8222,     8230,    8224,    8225,      // 80 to 87
     -1,      8240,    -1,      8249,    -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     -1,      8482,    -1,      8250,    -1,       -1,      -1,      -1,        // 98 to 9F
     160,     901,     902,     163,     164,      165,     166,     167,       // A0 to A7
     168,     169,     -1,      171,     172,      173,     174,     8213,      // A8 to AF
     176,     177,     178,     179,     900,      181,     182,     183,       // B0 to B7
     904,     905,     906,     187,     908,      189,     910,     911,       // B8 to BF
     912,     913,     914,     915,     916,      917,     918,     919,       // C0 to C7
     920,     921,     922,     923,     924,      925,     926,     927,       // C8 to CF
     928,     929,     -1,      931,     932,      933,     934,     935,       // D0 to D7
     936,     937,     938,     939,     940,      941,     942,     943,       // D8 to DF
     944,     945,     946,     947,     948,      949,     950,     951,       // E0 to E7
     952,     953,     954,     955,     956,      957,     958,     959,       // E8 to EF
     960,     961,     962,     963,     964,      965,     966,     967,       // F0 to F7
     968,     969,     970,     971,     972,      973,     974,     -1);           // F8 to FF

const
  cp1254_n: array[0..255] of string =
    ('.notdef',       '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',         'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',        'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',     'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',         'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',          'one',          'two',            'three',              // 30 to 33
     'four',          'five',         'six',            'seven',              // 34 to 37
     'eight',         'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',          'equal',        'greater',        'question',           // 3C to 3F
     'at',            'A',            'B',              'C',                  // 40 to 43
     'D',             'E',            'F',              'G',                  // 44 to 47
     'H',             'I',            'J',              'K',                  // 48 to 4B
     'L',             'M',            'N',              'O',                  // 4C to 4F
     'P',             'Q',            'R',              'S',                  // 50 to 53
     'T',             'U',            'V',              'W',                  // 54 to 57
     'X',             'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',     'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',         'a',            'b',              'c',                  // 60 to 63
     'd',             'e',            'f',              'g',                  // 64 to 67
     'h',             'i',            'j',              'k',                  // 68 to 6B
     'l',             'm',            'n',              'o',                  // 6C to 6F
     'p',             'q',            'r',              's',                  // 70 to 73
     't',             'u',            'v',              'w',                  // 74 to 77
     'x',             'y',            'z',              'braceleft',          // 78 to 7B
     'bar',           'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',          '.notdef',      'quotesinglbase', 'florin',             // 80 to 83
     'quotedblbase',  'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     'circumflex',    'perthousand',  'Scaron',         'guilsinglleft',      // 88 to 8B
     'OE',            '.notdef',      '.notdef',        '.notdef',            // 8C to 8F
     '.notdef',       'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright', 'bullet',       'endash',         'emdash',             // 94 to 97
     'tilde',         'trademark',    'scaron',         'guilsinglright',     // 98 to 9B
     'oe',            '.notdef',      '.notdef',        'Ydieresis',          // 9C to 9F
     'space',         'exclamdown',   'cent',           'sterling',           // A0 to A3
     'currency',      'yen',          'brokenbar',      'section',            // A4 to A7
     'dieresis',      'copyright',    'ordfeminine',    'guillemotleft',      // A8 to AB
     'logicalnot',    'hyphen',       'registered',     'macron',             // AC to AF
     'degree',        'plusminus',    'twosuperior',    'threesuperior',      // B0 to B3
     'acute',         'mu',           'paragraph',      'periodcentered',     // B4 to B7
       'cedilla',       'onesuperior','ordmasculine',   'guillemotright',     // B8 to BB
     'onequarter',    'onehalf',      'threequarters',  'questiondown',       // BC to BF
     'Agrave',        'Aacute',       'Acircumflex',    'Atilde',             // C0 to C3
     'Adieresis',     'Aring',        'AE',             'Ccedilla',           // C4 to C7
     'Egrave',        'Eacute',       'Ecircumflex',    'Edieresis',          // C8 to CB
     'Igrave',        'Iacute',       'Icircumflex',    'Idieresis',          // CC to CF
     'Gbreve',        'Ntilde',       'Ograve',         'Oacute',             // D0 to D3
     'Ocircumflex',   'Otilde',       'Odieresis',      'multiply',           // D4 to D7
     'Oslash',        'Ugrave',       'Uacute',         'Ucircumflex',        // D8 to DB
     'Udieresis',     'Idotaccent',   'Scedilla',       'germandbls',         // DC to DF
     'agrave',        'aacute',       'acircumflex',    'atilde',             // E0 to E3
     'adieresis',     'aring',        'ae',             'ccedilla',           // E4 to E7
     'egrave',        'eacute',       'ecircumflex',    'edieresis',          // E8 to EB
     'igrave',        'iacute',       'icircumflex',    'idieresis',          // EC to EF
     'gbreve',        'ntilde',       'ograve',         'oacute',             // F0 to F3
     'ocircumflex',   'otilde',       'odieresis',      'divide',             // F4 to F7
     'oslash',        'ugrave',       'uacute',         'ucircumflex',        // F8 to FB
     'udieresis',     'dotlessi',     'scedilla',       'ydieresis');         // FC to FF

const
  cp1254_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8364,    -1,      8218,    402,     8222,     8230,    8224,    8225,      // 80 to 87
     710,     8240,    352,     8249,    338,      -1,      -1,      -1,        // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     732,     8482,    353,     8250,    339,      -1,      -1,      376,       // 98 to 9F
     160,     161,     162,     163,     164,      165,     166,     167,       // A0 to A7
     168,     169,     170,     171,     172,      173,     174,     175,       // A8 to AF
     176,     177,     178,     179,     180,      181,     182,     183,       // B0 to B7
     184,     185,     186,     187,     188,      189,     190,     191,       // B8 to BF
     192,     193,     194,     195,     196,      197,     198,     199,       // C0 to C7
     200,     201,     202,     203,     204,      205,     206,     207,       // C8 to CF
     286,     209,     210,     211,     212,      213,     214,     215,       // D0 to D7
     216,     217,     218,     219,     220,      304,     350,     223,       // D8 to DF
     224,     225,     226,     227,     228,      229,     230,     231,       // E0 to E7
     232,     233,     234,     235,     236,      237,     238,     239,       // E8 to EF
     287,     241,     242,     243,     244,      245,     246,     247,       // F0 to F7
     248,     249,     250,     251,     252,      305,     351,     255);      // F8 to FF

const
  cp1255_n: array[0..255] of string =
    ('.notdef',         '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',           'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',          'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',       'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',           'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',            'one',          'two',            'three',              // 30 to 33
     'four',            'five',         'six',            'seven',              // 34 to 37
     'eight',           'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',            'equal',        'greater',        'question',           // 3C to 3F
     'at',              'A',            'B',              'C',                  // 40 to 43
     'D',               'E',            'F',              'G',                  // 44 to 47
     'H',               'I',            'J',              'K',                  // 48 to 4B
     'L',               'M',            'N',              'O',                  // 4C to 4F
     'P',               'Q',            'R',              'S',                  // 50 to 53
     'T',               'U',            'V',              'W',                  // 54 to 57
     'X',               'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',       'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',           'a',            'b',              'c',                  // 60 to 63
     'd',               'e',            'f',              'g',                  // 64 to 67
     'h',               'i',            'j',              'k',                  // 68 to 6B
     'l',               'm',            'n',              'o',                  // 6C to 6F
     'p',               'q',            'r',              's',                  // 70 to 73
     't',               'u',            'v',              'w',                  // 74 to 77
     'x',               'y',            'z',              'braceleft',          // 78 to 7B
     'bar',             'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',            '.notdef',      'quotesinglbase', 'florin',             // 80 to 83
     'quotedblbase',    'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     'circumflex',      'perthousand',  '.notdef',        'guilsinglleft',      // 88 to 8B
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 8C to 8F
     '.notdef',         'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright',   'bullet',       'endash',         'emdash',             // 94 to 97
     'tilde',           'trademark',    '.notdef',        'guilsinglright',     // 98 to 9B
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // 9C to 9F
     'space',           'exclamdown',   'cent',           'sterling',           // A0 to A3
     'afii57636',       'yen',          'brokenbar',      'section',            // A4 to A7
     'dieresis',        'copyright',    'multiply',       'guillemotleft',      // A8 to AB
     'logicalnot',      'sfthyphen',    'registered',     'macron',             // AC to AF
     'degree',          'plusminus',    'twosuperior',    'threesuperior',      // B0 to B3
     'acute',           'mu',           'paragraph',      'middot',             // B4 to B7
     'cedilla',         'onesuperior',  'divide',         'guillemotright',     // B8 to BB
     'onequarter',      'onehalf',      'threequarters',  'questiondown',       // BC to BF
     'afii57799',       'afii57801',    'afii57800',      'afii57802',          // C0 to C3
     'afii57793',       'afii57794',    'afii57795',      'afii57798',          // C4 to C7
     'afii57797',       'afii57806',    '.notdef',        'afii57796',          // C8 to CB
     'afii57807',       'afii57839',    'afii57645',      'afii57841',          // CC to CF
     'afii57842',       'afii57804',    'afii57803',      'afii57658',          // D0 to D3
     'afii57716',       'afii57717',    'afii57718',      'gereshhebrew',       // D4 to D7
     'gershayimhebrew', '.notdef',      '.notdef',        '.notdef',            // D8 to DB
     '.notdef',         '.notdef',      '.notdef',        '.notdef',            // DC to DF
     'afii57664',       'afii57665',    'afii57666',      'afii57667',          // E0 to E3
     'afii57668',       'afii57669',    'afii57670',      'afii57671',          // E4 to E7
     'afii57672',       'afii57673',    'afii57674',      'afii57675',          // E8 to EB
     'afii57676',       'afii57677',    'afii57678',      'afii57679',          // EC to EF
     'afii57680',       'afii57681',    'afii57682',      'afii57683',          // F0 to F3
     'afii57684',       'afii57685',    'afii57686',      'afii57687',          // F4 to F7
     'afii57688',       'afii57689',    'afii57690',      '.notdef',            // F8 to FB
     '.notdef',         'afii299',      'afii300',        '.notdef');           // FC to FF

const
  cp1255_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8364,    -1,      8218,    402,     8222,     8230,    8224,    8225,      // 80 to 87
     710,     8240,    -1,      8249,    -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     732,     8482,    -1,      8250,    -1,       -1,      -1,      -1,        // 98 to 9F
     160,     161,     162,     163,     8362,     165,     166,     167,       // A0 to A7
     168,     169,     215,     171,     172,      173,     174,     175,       // A8 to AF
     176,     177,     178,     179,     180,      181,     182,     183,       // B0 to B7
     184,     185,     247,     187,     188,      189,     190,     191,       // B8 to BF
     1456,    1457,    1458,    1459,    1460,     1461,    1462,    1463,      // C0 to C7
     1464,    1465,    -1,      1467,    1468,     1469,    1470,    1471,      // C8 to CF
     1472,    1473,    1474,    1475,    1520,     1521,    1522,    1523,      // D0 to D7
     1524,    -1,      -1,      -1,      -1,       -1,      -1,      -1,        // D8 to DF
     1488,    1489,    1490,    1491,    1492,     1493,    1494,    1495,      // E0 to E7
     1496,    1497,    1498,    1499,    1500,     1501,    1502,    1503,      // E8 to EF
     1504,    1505,    1506,    1507,    1508,     1509,    1510,    1511,      // F0 to F7
     1512,    1513,    1514,    -1,      -1,       8206,    8207,    -1);       // F8 to FF

const
  cp1257_n: array[0..255] of string =
    ('.notdef',       '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',         'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',        'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',     'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',         'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',          'one',          'two',            'three',              // 30 to 33
     'four',          'five',         'six',            'seven',              // 34 to 37
     'eight',         'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',          'equal',        'greater',        'question',           // 3C to 3F
     'at',            'A',            'B',              'C',                  // 40 to 43
     'D',             'E',            'F',              'G',                  // 44 to 47
     'H',             'I',            'J',              'K',                  // 48 to 4B
     'L',             'M',            'N',              'O',                  // 4C to 4F
     'P',             'Q',            'R',              'S',                  // 50 to 53
     'T',             'U',            'V',              'W',                  // 54 to 57
     'X',             'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',     'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',         'a',            'b',              'c',                  // 60 to 63
     'd',             'e',            'f',              'g',                  // 64 to 67
     'h',             'i',            'j',              'k',                  // 68 to 6B
     'l',             'm',            'n',              'o',                  // 6C to 6F
     'p',             'q',            'r',              's',                  // 70 to 73
     't',             'u',            'v',              'w',                  // 74 to 77
     'x',             'y',            'z',              'braceleft',          // 78 to 7B
     'bar',           'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',          '.notdef',      'quotesinglbase', '.notdef',            // 80 to 83
     'quotedblbase',  'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     '.notdef',       'perthousand',  '.notdef',        'guilsinglleft',      // 88 to 8B
     '.notdef',       'dieresis',     'caron',          'cedilla',            // 8C to 8F
     '.notdef',       'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright', 'bullet',       'endash',         'emdash',             // 94 to 97
     '.notdef',       'trademark',    '.notdef',        'guilsinglright',     // 98 to 9B
     '.notdef',       'macron',       'ogonek',         '.notdef',            // 9C to 9F
     'space',         '.notdef',      'cent',           'sterling',           // A0 to A3
     'currency',      '.notdef',      'brokenbar',      'section',            // A4 to A7
     'Oslash',        'copyright',    'Rcommaaccent',   'guillemotleft',      // A8 to AB
     'logicalnot',    'hyphen',       'registered',     'AE',                 // AC to AF
     'degree',        'plusminus',    'twosuperior',    'threesuperior',      // B0 to B3
     'acute',         'mu',           'paragraph',      'periodcentered',     // B4 to B7
     'oslash',        'onesuperior',  'rcommaaccent',   'guillemotright',     // B8 to BB
     'onequarter',    'onehalf',      'threequarters',  'ae',                 // BC to BF
     'Aogonek',       'Iogonek',      'Amacron',        'Cacute',             // C0 to C3
     'Adieresis',     'Aring',        'Eogonek',        'Emacron',            // C4 to C7
     'Ccaron',        'Eacute',       'Zacute',         'Edotaccent',         // C8 to CB
     'Gcommaaccent',  'Kcommaaccent', 'Imacron',        'Lcommaaccent',       // CC to CF
     'Scaron',        'Nacute',       'Ncommaaccent',   'Oacute',             // D0 to D3
     'Omacron',       'Otilde',       'Odieresis',      'multiply',           // D4 to D7
     'Uogonek',       'Lslash',       'Sacute',         'Umacron',            // D8 to DB
     'Udieresis',     'Zdotaccent',   'Zcaron',         'germandbls',         // DC to DF
     'aogonek',       'iogonek',      'amacron',        'cacute',             // E0 to E3
     'adieresis',     'aring',        'eogonek',        'emacron',            // E4 to E7
     'ccaron',        'eacute',       'zacute',         'edotaccent',         // E8 to EB
     'gcommaaccent',  'kcommaaccent', 'imacron',        'lcommaaccent',       // EC to EF
     'scaron',        'nacute',       'ncommaaccent',   'oacute',             // F0 to F3
     'omacron',       'otilde',       'odieresis',      'divide',             // F4 to F7
     'uogonek',       'lslash',       'sacute',         'umacron',            // F8 to FB
     'udieresis',     'zdotaccent',   'zcaron',         'dotaccent');         // FC to FF

const
  cp1257_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8364,    -1,      8218,    -1,      8222,     8230,    8224,    8225,      // 80 to 87
     -1,      8240,    -1,      8249,    -1,       168,     711,     184,       // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     -1,      8482,    -1,      8250,    -1,       175,     731,     -1,        // 98 to 9F
     160,     -1,      162,     163,     164,      -1,      166,     167,       // A0 to A7
     216,     169,     342,     171,     172,      173,     174,     198,       // A0 to AF
     176,     177,     178,     179,     180,      181,     182,     183,       // B0 to B7
     248,     185,     343,     187,     188,      189,     190,     230,       // B8 to BF
     260,     302,     256,     262,     196,      197,     280,     274,       // C0 to C7
     268,     201,     377,     278,     290,      310,     298,     315,       // C8 to CF
     352,     323,     325,     211,     332,      213,     214,     215,       // D0 to D7
     370,     321,     346,     362,     220,      379,     381,     223,       // D8 to DF
     261,     303,     257,     263,     228,      229,     281,     275,       // E0 to E7
     269,     233,     378,     279,     291,      311,     299,     316,       // E8 to EF
     353,     324,     326,     243,     333,      245,     246,     247,       // F0 to F7
     371,     322,     347,     363,     252,      380,     382,     729);      // F8 to FF

const
  cp1258_n: array[0..255] of string =
    ('.notdef',       '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',       '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',         'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',        'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',     'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',         'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',          'one',          'two',            'three',              // 30 to 33
     'four',          'five',         'six',            'seven',              // 34 to 37
     'eight',         'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',          'equal',        'greater',        'question',           // 3C to 3F
     'at',            'A',            'B',              'C',                  // 40 to 43
     'D',             'E',            'F',              'G',                  // 44 to 47
     'H',             'I',            'J',              'K',                  // 48 to 4B
     'L',             'M',            'N',              'O',                  // 4C to 4F
     'P',             'Q',            'R',              'S',                  // 50 to 53
     'T',             'U',            'V',              'W',                  // 54 to 57
     'X',             'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash',     'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',         'a',            'b',              'c',                  // 60 to 63
     'd',             'e',            'f',              'g',                  // 64 to 67
     'h',             'i',            'j',              'k',                  // 68 to 6B
     'l',             'm',            'n',              'o',                  // 6C to 6F
     'p',             'q',            'r',              's',                  // 70 to 73
     't',             'u',            'v',              'w',                  // 74 to 77
     'x',             'y',            'z',              'braceleft',          // 78 to 7B
     'bar',           'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'Euro',          '.notdef',      'quotesinglbase', 'florin',             // 80 to 83
     'quotedblbase',  'ellipsis',     'dagger',         'daggerdbl',          // 84 to 87
     'circumflex',    'perthousand',  '.notdef',        'guilsinglleft',      // 88 to 8B
     'OE',            '.notdef',      '.notdef',        '.notdef',            // 8C to 8F
     '.notdef',       'quoteleft',    'quoteright',     'quotedblleft',       // 90 to 93
     'quotedblright', 'bullet',       'endash',         'emdash',             // 94 to 97
     'tilde',         'trademark',    '.notdef',        'guilsinglright',     // 98 to 9B
     'oe',            '.notdef',      '.notdef',        'Ydieresis',          // 9C to 9F
     'space',         'exclamdown',   'cent',           'sterling',           // A0 to A3
     'currency',      'yen',          'brokenbar',      'section',            // A4 to A7
     'dieresis',      'copyright',    'ordfeminine',    'guillemotleft',      // A8 to AB
     'logicalnot',    'hyphen',       'registered',     'macron',             // AC to AF
     'degree',        'plusminus',    'twosuperior',    'threesuperior',      // B0 to B3
     'acute',         'mu',           'paragraph',             'periodcentered',     // B4 to B7
     'cedilla',       'onesuperior',  'ordmasculine',   'guillemotright',     // B8 to BB
     'onequarter',    'onehalf',      'threequarters',  'questiondown',       // BC to BF
     'Agrave',        'Aacute',       'Acircumflex',    'Abreve',             // C0 to C3
     'Adieresis',     'Aring',        'AE',             'Ccedilla',           // C4 to C7
     'Egrave',        'Eacute',       'Ecircumflex',    'Edieresis',          // C8 to CB
     'gravecomb',     'Iacute',       'Icircumflex',    'Idieresis',          // CC to CF
     'Dcroat',        'Ntilde',       'hookabovecomb',  'Oacute',             // D0 to D3
     'Ocircumflex',   'Ohorn',        'Odieresis',      'multiply',           // D4 to D7
     'Oslash',        'Ugrave',       'Uacute',         'Ucircumflex',        // D8 to DB
     'Udieresis',     'Uhorn',        'tildecomb',      'germandbls',         // DC to DF
     'agrave',        'aacute',       'acircumflex',    'abreve',             // E0 to E3
     'adieresis',     'aring',        'ae',             'ccedilla',           // E4 to E7
     'egrave',        'eacute',       'ecircumflex',    'edieresis',          // E8 to EB
     'acutecomb',     'iacute',       'icircumflex',    'idieresis',          // EC to EF
     'dcroat',        'ntilde',       'dotbelowcomb',   'oacute',             // F0 to F3
     'ocircumflex',   'ohorn',        'odieresis',      'divide',             // F4 to F7
     'oslash',        'ugrave',       'uacute',         'ucircumflex',        // F8 to FB
     'udieresis',     'uhorn',        'dong',           'ydieresis');         // FC to FF

const
  cp1258_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     8264,    -1,      8218,    402,     8222,     8230,    8224,    8225,      // 80 to 87
     710,     8240,    -1,      8249,    338,      -1,      -1,      -1,        // 88 to 8F
     -1,      8216,    8217,    8220,    8221,     8226,    8211,    8212,      // 90 to 97
     732,     8482,    -1,      8250,    339,      -1,      -1,      376,       // 98 to 9F
     160,     161,     162,     163,     164,      165,     166,     167,       // A0 to A7
     168,     169,     170,     171,     172,      173,     174,     175,       // A8 to AF
     176,     177,     178,     179,     180,      181,     182,     183,       // B0 to B7
     184,     185,     186,     187,     188,      189,     190,     191,       // B8 to BF
     192,     193,     194,     258,     196,      197,     198,     199,       // C0 to C7
     200,     201,     202,     203,     768,      205,     206,     207,       // C8 to CF
     272,     209,     777,     211,     212,      416,     214,     215,       // D0 to D7
     216,     217,     218,     219,     220,      431,     771,     223,       // D8 to DF
     224,     225,     226,     259,     228,      229,     230,     231,       // E0 to E7
     232,     233,     234,     235,     769,      237,     238,     239,       // E8 to EF
     273,     241,     803,     243,     244,      417,     246,     247,       // F0 to F7
     248,     249,     250,     251,     252,      432,     8363,    255);      // F8 to FF

const
  iso_8859_1_n: array[0..255] of string =
    ('.notdef',     '.notdef',      '.notdef',      '.notdef',            // 00 to 03
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 04 to 07
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 08 to 0B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 0C to 0F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 10 to 13
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 14 to 17
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 18 to 1B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 1C to 1F
     'space',       'exclam',       'quotedbl',     'numbersign',         // 20 to 23
     'dollar',      'percent',      'ampersand',    'quotesingle',        // 24 to 27
     'parenleft',   'parenright',   'asterisk',     'plus',               // 28 to 2B
     'comma',       'hyphen',       'period',       'slash',              // 2C to 2F
     'zero',        'one',          'two',          'three',              // 30 to 33
     'four',        'five',         'six',          'seven',              // 34 to 37
     'eight',       'nine',         'colon',        'semicolon',          // 38 to 3B
     'less',        'equal',        'greater',      'question',           // 3C to 3F
     'at',          'A',            'B',            'C',                  // 40 to 43
     'D',           'E',            'F',            'G',                  // 44 to 47
     'H',           'I',            'J',            'K',                  // 48 to 4B
     'L',           'M',            'N',            'O',                  // 4C to 4F
     'P',           'Q',            'R',            'S',                  // 50 to 53
     'T',           'U',            'V',            'W',                  // 54 to 57
     'X',           'Y',            'Z',            'bracketleft',        // 58 to 5B
     'backslash',   'bracketright', 'asciicircum',  'underscore',         // 5C to 5F
     'grave',       'a',            'b',            'c',                  // 60 to 63
     'd',           'e',            'f',            'g',                  // 64 to 67
     'h',           'i',            'j',            'k',                  // 68 to 6B
     'l',           'm',            'n',            'o',                  // 6C to 6F
     'p',           'q',            'r',            's',                  // 70 to 73
     't',           'u',            'v',            'w',                  // 74 to 77
     'x',           'y',            'z',            'braceleft',          // 78 to 7B
     'bar',         'braceright',   'asciitilde',   '.notdef',            // 7C to 7F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 80 to 83
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 84 to 87
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 88 to 8B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 8C to 8F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 90 to 93
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 94 to 97
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 98 to 9B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 9C to 9F
     'space',       'exclamdown',   'cent',         'sterling',           // A0 to A3
     'currency',    'yen',          'brokenbar',    'section',            // A4 to A7
     'dieresis',    'copyright',    'ordfeminine',  'guillemotleft',      // A8 to AB
     'logicalnot',  'hyphen',       'registered',   'macron',             // AC to AF
     'degree',      'plusminus',    'twosuperior',  'threesuperior',      // B0 to B3
     'acute',       'mu',           'paragraph',    'periodcentered',     // B4 to B7
     'cedilla',     'onesuperior',  'ordmasculine', 'guillemotright',     // B8 to BB
     'onequarter',  'onehalf',      'threequarters','questiondown',       // BC to BF
     'Agrave',      'Aacute',       'Acircumflex',  'Atilde',             // C0 to C3
     'Adieresis',   'Aring',        'AE',           'Ccedilla',           // C4 to C7
     'Egrave',      'Eacute',       'Ecircumflex',  'Edieresis',          // C8 to CB
     'Igrave',      'Iacute',       'Icircumflex',  'Idieresis',          // CC to CF
     'Eth',         'Ntilde',       'Ograve',       'Oacute',             // D0 to D3
     'Ocircumflex', 'Otilde',       'Odieresis',    'multiply',           // D4 to D7
     'Oslash',      'Ugrave',       'Uacute',       'Ucircumflex',        // D8 to DB
     'Udieresis',   'Yacute',       'Thorn',        'germandbls',         // DC to DF
     'agrave',      'aacute',       'acircumflex',  'atilde',             // E0 to E3
     'adieresis',   'aring',        'ae',           'ccedilla',           // E4 to E7
     'egrave',      'eacute',       'ecircumflex',  'edieresis',          // E8 to EB
     'igrave',      'iacute',       'icircumflex',  'idieresis',          // EC to EF
     'eth',         'ntilde',       'ograve',       'oacute',             // F0 to F3
     'ocircumflex', 'otilde',       'odieresis',    'divide',             // F4 to F7
     'oslash',      'ugrave',       'uacute',       'ucircumflex',        // F8 to FB
     'udieresis',   'yacute',       'thorn',        'ydieresis');         // FC to FF

const
  iso_8859_1_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     161,     162,     163,     164,      165,     166,     167,       // A0 to A7
     168,     169,     170,     171,     172,      173,     174,     175,       // A8 to AF
     176,     177,     178,     179,     180,      181,     182,     183,       // B0 to B7
     184,     185,     186,     187,     188,      189,     190,     191,       // B8 to BF
     192,     193,     194,     195,     196,      197,     198,     199,       // C0 to C7
     200,     201,     202,     203,     204,      205,     206,     207,       // C8 to CF
     208,     209,     210,     211,     212,      213,     214,     215,       // D0 to D7
     216,     217,     218,     219,     220,      221,     222,     223,       // D8 to DF
     224,     225,     226,     227,     228,      229,     230,     231,       // E0 to E7
     232,     233,     234,     235,     236,      237,     238,     239,       // E8 to EF
     240,     241,     242,     243,     244,      245,     246,     247,       // F0 to F7
     248,     249,     250,     251,     252,      253,     254,     255);      // F8 to FF

const
  iso_8859_2_n: array[0..255] of string =
    ('.notdef',     '.notdef',      '.notdef',      '.notdef',            // 00 to 03
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 04 to 07
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 08 to 0B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 0C to 0F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 10 to 13
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 14 to 17
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 18 to 1B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 1C to 1F
     'space',       'exclam',       'quotedbl',     'numbersign',         // 20 to 23
     'dollar',      'percent',      'ampersand',    'quotesingle',        // 24 to 27
     'parenleft',   'parenright',   'asterisk',     'plus',               // 28 to 2B
     'comma',       'hyphen',       'period',       'slash',              // 2C to 2F
     'zero',        'one',          'two',          'three',              // 30 to 33
     'four',        'five',         'six',          'seven',              // 34 to 37
     'eight',       'nine',         'colon',        'semicolon',          // 38 to 3B
     'less',        'equal',        'greater',      'question',           // 3C to 3F
     'at',          'A',            'B',            'C',                  // 40 to 43
     'D',           'E',            'F',            'G',                  // 44 to 47
     'H',           'I',            'J',            'K',                  // 48 to 4B
     'L',           'M',            'N',            'O',                  // 4C to 4F
     'P',           'Q',            'R',            'S',                  // 50 to 53
     'T',           'U',            'V',            'W',                  // 54 to 57
     'X',           'Y',            'Z',            'bracketleft',        // 58 to 5B
     'backslash',   'bracketright', 'asciicircum',  'underscore',         // 5C to 5F
     'grave',       'a',            'b',            'c',                  // 60 to 63
     'd',           'e',            'f',            'g',                  // 64 to 67
     'h',           'i',            'j',            'k',                  // 68 to 6B
     'l',           'm',            'n',            'o',                  // 6C to 6F
     'p',           'q',            'r',            's',                  // 70 to 73
     't',           'u',            'v',            'w',                  // 74 to 77
     'x',           'y',            'z',            'braceleft',          // 78 to 7B
     'bar',         'braceright',   'asciitilde',   '.notdef',            // 7C to 7F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 80 to 83
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 84 to 87
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 88 to 8B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 8C to 8F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 90 to 93
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 94 to 97
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 98 to 9B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 9C to 9F
     'space',       'Aogonek',      'breve',        'Lslash',             // A0 to A3
     'currency',    'Lcaron',       'Sacute',       'section',            // A4 to A7
     'dieresis',    'Scaron',       'Scedilla',     'Tcaron',             // A8 to AB
     'Zacute',      'hyphen',       'Zcaron',       'Zdotaccent',         // AC to AF
     'degree',      'aogonek',      'ogonek',       'lslash',             // B0 to B3
     'acute',       'lcaron',       'sacute',       'caron',              // B4 to B7
     'cedilla',     'scaron',       'scedilla',     'tcaron',             // B8 to BB
     'zacute',      'hungarumlaut', 'zcaron',       'zdotaccent',         // BC to BF
     'Racute',      'Aacute',       'Acircumflex',  'Abreve',             // C0 to C3
     'Adieresis',   'Lacute',       'Cacute',       'Ccedilla',           // C4 to C7
     'Ccaron',      'Eacute',       'Eogonek',      'Edieresis',          // C8 to CB
     'Ecaron',      'Iacute',       'Icircumflex',  'Dcaron',             // CC to CF
     'Dcroat',      'Nacute',       'Ncaron',       'Oacute',             // D0 to D3
     'Ocircumflex', 'Ohungarumlaut','Odieresis',    'multiply',           // D4 to D7
     'Rcaron',      'Uring',        'Uacute',       'Uhungarumlaut',      // D8 to DB
     'Udieresis',   'Yacute',       'Tcommaaccent', 'germandbls',         // DC to DF
     'racute',      'aacute',       'acircumflex',  'abreve',             // E0 to E3
     'adieresis',   'lacute',       'cacute',       'ccedilla',           // E4 to E7
     'ccaron',      'eacute',       'eogonek',      'edieresis',          // E8 to EB
     'ecaron',      'iacute',       'icircumflex',  'dcaron',             // EC to EF
     'dcroat',      'nacute',       'ncaron',       'oacute',             // F0 to F3
     'ocircumflex', 'ohungarumlaut','odieresis',    'divide',             // F4 to F7
     'rcaron',      'uring',        'uacute',       'uhungarumlaut',      // F8 to FB
     'udieresis',   'yacute',       'tcommaaccent', 'dotaccent');         // FC to FF

const
  iso_8859_2_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     260,     728,     321,     164,      317,     346,     167,       // A0 to A7
     168,     352,     350,     356,     377,      173,     381,     379,       // A8 to AF
     176,     261,     731,     322,     180,      318,     347,     711,       // B0 to B7
     184,     353,     351,     357,     378,      733,     382,     380,       // B8 to BF
     340,     193,     194,     258,     196,      313,     262,     199,       // C0 to C7
     268,     201,     280,     203,     282,      205,     206,     270,       // C8 to CF
     272,     323,     327,     211,     212,      336,     214,     215,       // D0 to D7
     344,     366,     218,     368,     220,      221,     354,     223,       // D8 to DF
     341,     225,     226,     259,     228,      314,     263,     231,       // E0 to E7
     269,     233,     281,     235,     283,      237,     238,     271,       // E8 to EF
     273,     324,     328,     243,     244,      337,     246,     247,       // F0 to F7
     345,     367,     250,     369,     252,      253,     355,     729);      // F8 to FF

const
  iso_8859_4_n: array[0..255] of string =
    ('.notdef',     '.notdef',      '.notdef',      '.notdef',            // 00 to 03
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 04 to 07
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 08 to 0B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 0C to 0F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 10 to 13
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 14 to 17
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 18 to 1B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 1C to 1F
     'space',       'exclam',       'quotedbl',     'numbersign',         // 20 to 23
     'dollar',      'percent',      'ampersand',    'quotesingle',        // 24 to 27
     'parenleft',   'parenright',   'asterisk',     'plus',               // 28 to 2B
     'comma',       'hyphen',       'period',       'slash',              // 2C to 2F
     'zero',        'one',          'two',          'three',              // 30 to 33
     'four',        'five',         'six',          'seven',              // 34 to 37
     'eight',       'nine',         'colon',        'semicolon',          // 38 to 3B
     'less',        'equal',        'greater',      'question',           // 3C to 3F
     'at',          'A',            'B',            'C',                  // 40 to 43
     'D',           'E',            'F',            'G',                  // 44 to 47
     'H',           'I',            'J',            'K',                  // 48 to 4B
     'L',           'M',            'N',            'O',                  // 4C to 4F
     'P',           'Q',            'R',            'S',                  // 50 to 53
     'T',           'U',            'V',            'W',                  // 54 to 57
     'X',           'Y',            'Z',            'bracketleft',        // 58 to 5B
     'backslash',   'bracketright', 'asciicircum',  'underscore',         // 5C to 5F
     'grave',       'a',            'b',            'c',                  // 60 to 63
     'd',           'e',            'f',            'g',                  // 64 to 67
     'h',           'i',            'j',            'k',                  // 68 to 6B
     'l',           'm',            'n',            'o',                  // 6C to 6F
     'p',           'q',            'r',            's',                  // 70 to 73
     't',           'u',            'v',            'w',                  // 74 to 77
     'x',           'y',            'z',            'braceleft',          // 78 to 7B
     'bar',         'braceright',   'asciitilde',   '.notdef',            // 7C to 7F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 80 to 83
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 84 to 87
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 88 to 8B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 8C to 8F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 90 to 93
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 94 to 97
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 98 to 9B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 9C to 9F
     'space',       'Aogonek',      'kgreenlandic', 'Rcommaaccent',       // A0 to A3
     'currency',    'Itilde',       'Lcommaaccent', 'section',            // A4 to A7
     'dieresis',    'Scaron',       'Emacron',      'Gcommaaccent',       // A8 to AB
     'Tbar',        'hyphen',       'Zcaron',       'macron',             // AC to AF
     'degree',      'aogonek',      'ogonek',       'rcommaaccent',       // B0 to B3
     'acute',       'itilde',       'lcommaaccent', 'caron',              // B4 to B7
     'cedilla',     'scaron',       'emacron',      'gcommaaccent',       // B8 to BB
     'tbar',        'Eng',          'zcaron',       'eng',                // BC to BF
     'Amacron',     'Aacute',       'Acircumflex',  'Atilde',             // C0 to C3
     'Adieresis',   'Aring',        'AE',           'Iogonek',            // C4 to C7
     'Ccaron',      'Eacute',       'Eogonek',      'Edieresis',          // C8 to CB
     'Edotaccent',  'Iacute',       'Icircumflex',  'Imacron',            // CC to CF
     'Dcroat',      'Ncommaaccent', 'Omacron',      'Kcommaaccent',       // D0 to D3
     'Ocircumflex', 'Otilde',       'Odieresis',    'multiply',           // D4 to D7
     'Oslash',      'Uogonek',      'Uacute',       'Ucircumflex',        // D8 to DB
     'Udieresis',   'Utilde',       'Umacron',      'germandbls',         // DC to DF
     'amacron',     'aacute',       'acircumflex',  'atilde',             // E0 to E3
     'adieresis',   'aring',        'ae',           'iogonek',            // E4 to E7
     'ccaron',      'eacute',       'eogonek',      'edieresis',          // E8 to EB
     'edotaccent',  'iacute',       'icircumflex',  'imacron',            // EC to EF
     'dcroat',      'ncommaaccent', 'omacron',      'kcommaaccent',       // F0 to F3
     'ocircumflex', 'otilde',       'odieresis',    'divide',             // F4 to F7
     'oslash',      'uogonek',      'uacute',       'ucircumflex',        // F8 to FB
     'udieresis',   'utilde',       'umacron',      'dotaccent');         // FC to FF

const
  iso_8859_4_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     260,     312,     342,     164,      296,     315,     167,       // A4 to A7
     168,     352,     274,     290,     358,      173,     381,     175,       // AC to AF
     176,     261,     731,     343,     180,      297,     316,     711,       // B4 to B7
     184,     353,     275,     291,     359,      330,     382,     331,       // BC to BF
     256,     193,     194,     195,     196,      197,     198,     302,       // C4 to C7
     268,     201,     280,     203,     278,      205,     206,     298,       // CC to CF
     272,     325,     332,     310,     212,      213,     214,     215,       // D4 to D7
     216,     370,     218,     219,     220,      360,     362,     223,       // DC to DF
     257,     225,     226,     227,     228,      229,     230,     303,       // E4 to E7
     269,     233,     281,     235,     279,      237,     238,     299,       // EC to EF
     273,     326,     333,     311,     244,      245,     246,     247,       // F4 to F7
     248,     371,     250,     251,     252,      361,     363,     729);      // FC to FF

const
  iso_8859_5_n: array[0..255] of string =
    ('.notdef',   '.notdef',      '.notdef',    '.notdef',            // 00 to 03
     '.notdef',   '.notdef',      '.notdef',    '.notdef',            // 04 to 07
     '.notdef',   '.notdef',      '.notdef',    '.notdef',            // 08 to 0B
     '.notdef',   '.notdef',      '.notdef',    '.notdef',            // 0C to 0F
     '.notdef',   '.notdef',      '.notdef',    '.notdef',            // 10 to 13
     '.notdef',   '.notdef',      '.notdef',    '.notdef',            // 14 to 17
     '.notdef',   '.notdef',      '.notdef',    '.notdef',            // 18 to 1B
     '.notdef',   '.notdef',      '.notdef',    '.notdef',            // 1C to 1F
     'space',     'exclam',       'quotedbl',   'numbersign',         // 20 to 23
     'dollar',    'percent',      'ampersand',  'quotesingle',        // 24 to 27
     'parenleft', 'parenright',   'asterisk',   'plus',               // 28 to 2B
     'comma',     'hyphen',       'period',     'slash',              // 2C to 2F
     'zero',      'one',          'two',        'three',              // 30 to 33
     'four',      'five',         'six',        'seven',              // 34 to 37
     'eight',     'nine',         'colon',      'semicolon',          // 38 to 3B
     'less',      'equal',        'greater',    'question',           // 3C to 3F
     'at',        'A',            'B',          'C',                  // 40 to 43
     'D',         'E',            'F',          'G',                  // 44 to 47
     'H',         'I',            'J',          'K',                  // 48 to 4B
     'L',         'M',            'N',          'O',                  // 4C to 4F
     'P',         'Q',            'R',          'S',                  // 50 to 53
     'T',         'U',            'V',          'W',                  // 54 to 57
     'X',         'Y',            'Z',          'bracketleft',        // 58 to 5B
     'backslash', 'bracketright', 'asciicircum','underscore',         // 5C to 5F
     'grave',     'a',            'b',          'c',                  // 60 to 63
     'd',         'e',            'f',          'g',                  // 64 to 67
     'h',         'i',            'j',          'k',                  // 68 to 6B
     'l',         'm',            'n',          'o',                  // 6C to 6F
     'p',         'q',            'r',          's',                  // 70 to 73
     't',         'u',            'v',          'w',                  // 74 to 77
     'x',         'y',            'z',          'braceleft',          // 78 to 7B
     'bar',       'braceright',   'asciitilde', '.notdef',            // 7C to 7F
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 80 to 83
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 84 to 87
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 88 to 8B
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 8C to 8F
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 90 to 93
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 94 to 97
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 98 to 9B
     '.notdef',   '.notdef',      '.notdef',    '.notdef',     // 9C to 9F
     'space',     'afii10023',    'afii10051',  'afii10052',     // A0 to A3
     'afii10053', 'afii10054',    'afii10055',  'afii10056',     // A4 to A7
     'afii10057', 'afii10058',    'afii10059',  'afii10060',     // A8 to AB
     'afii10061', 'hyphen',       'afii10062',  'afii10145',     // AC to AF
     'afii10017', 'afii10018',    'afii10019',  'afii10020',     // B0 to B3
     'afii10021', 'afii10022',    'afii10024',  'afii10025',     // B4 to B7
     'afii10026', 'afii10027',    'afii10028',  'afii10029',     // B8 to BB
     'afii10030', 'afii10031',    'afii10032',  'afii10033',     // BC to BF
     'afii10034', 'afii10035',    'afii10036',  'afii10037',     // C0 to C3
     'afii10038', 'afii10039',    'afii10040',  'afii10041',     // C4 to C7
     'afii10042', 'afii10043',    'afii10044',  'afii10045',     // C8 to CB
     'afii10046', 'afii10047',    'afii10048',  'afii10049',     // CC to CF
     'afii10065', 'afii10065',    'afii10067',  'afii10068',     // D0 to D3
     'afii10069', 'afii10070',    'afii10072',  'afii10073',     // D4 to D7
     'afii10074', 'afii10075',    'afii10076',  'afii10077',     // D8 to DB
     'afii10078', 'afii10079',    'afii10080',  'afii10081',     // DC to DF
     'afii10082', 'afii10083',    'afii10084',  'afii10085',     // E0 to E3
     'afii10086', 'afii10087',    'afii10088',  'afii10089',     // E4 to E7
     'afii10090', 'afii10091',    'afii10092',  'afii10093',     // E8 to EB
     'afii10094', 'afii10095',    'afii10096',  'afii10097',     // EC to EF
     'afii61352', 'afii10071',    'afii10099',  'afii10100',     // F0 to F3
     'afii10101', 'afii10102',    'afii10103',  'afii10104',     // F4 to F7
     'afii10105', 'afii10106',    'afii10107',  'afii10108',     // F8 to FB
     'afii10109', 'section',      'afii10110',  'afii10193');    // FC to FF

const
  iso_8859_5_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     1025,    1026,    1027,    1028,     1029,    1030,    1031,      // A0 to A7
     1032,    1033,    1034,    1035,    1036,     173,     1038,    1039,      // A8 to AF
     1040,    1041,    1042,    1043,    1044,     1045,    1046,    1047,      // B0 to B7
     1048,    1049,    1050,    1051,    1052,     1053,    1054,    1055,      // B8 to BF
     1056,    1057,    1058,    1059,    1060,     1061,    1062,    1063,      // C0 to C7
     1064,    1065,    1066,    1067,    1068,     1069,    1070,    1071,      // C8 to CF
     1072,    1073,    1074,    1075,    1076,     1077,    1078,    1079,      // D0 to D7
     1080,    1081,    1082,    1083,    1084,     1085,    1086,    1087,      // D8 to DF
     1088,    1089,    1090,    1091,    1092,     1093,    1094,    1095,      // E0 to E7
     1096,    1097,    1098,    1099,    1100,     1101,    1102,    1103,      // E8 to EF
     8470,    1105,    1106,    1107,    1108,     1109,    1110,    1111,      // F0 to F7
     1112,    1113,    1114,    1115,    1116,     167,     1118,    1119);     // F8 to FF

const
  iso_8859_7_n: array[0..255] of string =
    ('.notdef',           '.notdef',      '.notdef',        '.notdef',                // 00 to 03
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 04 to 07
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 08 to 0B
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 0C to 0F
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 10 to 13
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 14 to 17
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 18 to 1B
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 1C to 1F
     'space',             'exclam',       'quotedbl',       'numbersign',             // 20 to 23
     'dollar',            'percent',      'ampersand',      'quotesingle',            // 24 to 27
     'parenleft',         'parenright',   'asterisk',       'plus',                   // 28 to 2B
     'comma',             'hyphen',       'period',         'slash',                  // 2C to 2F
     'zero',              'one',          'two',            'three',                  // 30 to 33
     'four',              'five',         'six',            'seven',                  // 34 to 37
     'eight',             'nine',         'colon',          'semicolon',              // 38 to 3B
     'less',              'equal',        'greater',        'question',               // 3C to 3F
     'at',                'A',            'B',              'C',                      // 40 to 43
     'D',                 'E',            'F',              'G',                      // 44 to 47
     'H',                 'I',            'J',              'K',                      // 48 to 4B
     'L',                 'M',            'N',              'O',                      // 4C to 4F
     'P',                 'Q',            'R',              'S',                      // 50 to 53
     'T',                 'U',            'V',              'W',                      // 54 to 57
     'X',                 'Y',            'Z',              'bracketleft',            // 58 to 5B
     'backslash',         'bracketright', 'asciicircum',    'underscore',             // 5C to 5F
     'grave',             'a',            'b',              'c',                      // 60 to 63
     'd',                 'e',            'f',              'g',                      // 64 to 67
     'h',                 'i',            'j',              'k',                      // 68 to 6B
     'l',                 'm',            'n',              'o',                      // 6C to 6F
     'p',                 'q',            'r',              's',                      // 70 to 73
     't',                 'u',            'v',              'w',                      // 74 to 77
     'x',                 'y',            'z',              'braceleft',              // 78 to 7B
     'bar',               'braceright',   'asciitilde',     '.notdef',                // 7C to 7F
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 80 to 83
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 84 to 87
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 88 to 8B
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 8C to 8F
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 90 to 93
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 94 to 97
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 98 to 9B
     '.notdef',           '.notdef',      '.notdef',        '.notdef',                // 9C to 9F
     'space',             'quoteleft',    'quoteright',     'sterling',               // A0 to A3
     'brokenbar',         'section',      'dieresis',       'copyright',              // A4 to A7
     'guillemotleft',     'logicalnot',   'hyphen',         'afii00208',              // A8 to AB
     'degree',            'plusminus',    'twosuperior',    'threesuperior',          // AC to AF
     'tonos',             'dieresistonos','Alphatonos',     'periodcentered',         // B0 to B3
     'Epsilontonos',      'Etatonos',     'Iotatonos',      'guillemotright',         // B4 to B7
     'Omicrontonos',      'onehalf',      'Upsilontonos',   'Omegatonos',             // B8 to BB
     'iotadieresistonos', 'Alpha',        'Beta',           'Gamma',                  // BC to BF
     'Delta',             'Epsilon',      'Zeta',           'Eta',                    // C0 to C3
     'Theta',             'Iota',         'Kappa',          'Lambda',                 // C4 to C7
     'Mu',                'Nu',           'Xi',             'Omicron',                // C8 to CB
     'Pi',                'Rho',          'Sigma',          'Tau',                    // CC to CF
     'Upsilon',           'Phi',          'Chi',            'Psi',                    // D0 to D3
     'Omega',             'Iotadieresis', 'Upsilondieresis','alphatonos',             // D4 to D7
     'epsilontonos',      'etatonos',     'iotatonos',      'upsilondieresistonos',   // D8 to DB
     'alpha',             'beta',         'gamma',          'delta',                  // DC to DF
     'epsilon',           'zeta',         'eta',            'theta',                  // E0 to E3
     'iota',              'kappa',        'lambda',         'mu',                     // E4 to E7
     'nu',                'xi',           'omicron',        'pi',                     // E8 to EB
     'rho',               'sigma1',       'sigma',          'tau',                    // EC to EF
     'upsilon',           'phi',          'chi',            'psi',                    // F0 to F3
     'omega',             'iotadieresis', 'upsilondieresis','omicrontonos',           // F4 to F7
     'upsilontonos',      'omegatonos',   '.notdef',        '.notdef',                // F8 to FB
     '.notdef',           '.notdef',      '.notdef',        '.notdef');               // FC to FF

const
  iso_8859_7_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     8216,    8217,    163,     166,      167,     168,     169,       // A0 to A7
     171,     172,     173,     8213,    176,      177,     178,     179,       // A8 to AF
     900,     901,     902,     183,     904,      905,     906,     187,       // B0 to B7
     908,     189,     910,     911,     912,      913,     914,     915,       // B8 to BF
     916,     917,     918,     919,     920,      921,     922,     923,       // C0 to C7
     924,     925,     926,     927,     928,      929,     931,     932,       // C8 to CF
     933,     934,     935,     936,     937,      938,     939,     940,       // D0 to D7
     941,     942,     943,     944,     945,      946,     947,     948,       // D8 to DF
     949,     950,     951,     952,     953,      954,     955,     956,       // E0 to E7
     957,     958,     959,     960,     961,      962,     963,     964,       // E8 to EF
     965,     966,     967,     968,     969,      970,     971,     972,       // F0 to F7
     973,     974,     -1,      -1,      -1,       -1,      -1,      -1);       // F8 to FF

const
  iso_8859_9_n: array[0..255] of string =
    ('.notdef',     '.notdef',      '.notdef',      '.notdef',            // 00 to 03
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 04 to 07
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 08 to 0B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 0C to 0F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 10 to 13
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 14 to 17
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 18 to 1B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 1C to 1F
     'space',       'exclam',       'quotedbl',     'numbersign',         // 20 to 23
     'dollar',      'percent',      'ampersand',    'quotesingle',        // 24 to 27
     'parenleft',   'parenright',   'asterisk',     'plus',               // 28 to 2B
     'comma',       'hyphen',       'period',       'slash',              // 2C to 2F
     'zero',        'one',          'two',          'three',              // 30 to 33
     'four',        'five',         'six',          'seven',              // 34 to 37
     'eight',       'nine',         'colon',        'semicolon',          // 38 to 3B
     'less',        'equal',        'greater',      'question',           // 3C to 3F
     'at',          'A',            'B',            'C',                  // 40 to 43
     'D',           'E',            'F',            'G',                  // 44 to 47
     'H',           'I',            'J',            'K',                  // 48 to 4B
     'L',           'M',            'N',            'O',                  // 4C to 4F
     'P',           'Q',            'R',            'S',                  // 50 to 53
     'T',           'U',            'V',            'W',                  // 54 to 57
     'X',           'Y',            'Z',            'bracketleft',        // 58 to 5B
     'backslash',   'bracketright', 'asciicircum',  'underscore',         // 5C to 5F
     'grave',       'a',            'b',            'c',                  // 60 to 63
     'd',           'e',            'f',            'g',                  // 64 to 67
     'h',           'i',            'j',            'k',                  // 68 to 6B
     'l',           'm',            'n',            'o',                  // 6C to 6F
     'p',           'q',            'r',            's',                  // 70 to 73
     't',           'u',            'v',            'w',                  // 74 to 77
     'x',           'y',            'z',            'braceleft',          // 78 to 7B
     'bar',         'braceright',   'asciitilde',   '.notdef',            // 7C to 7F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 80 to 83
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 84 to 87
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 88 to 8B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 8C to 8F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 90 to 93
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 94 to 97
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 98 to 9B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 9C to 9F
     'space',       'exclamdown',   'cent',         'sterling',           // A0 to A3
     'currency',    'yen',          'brokenbar',    'section',            // A4 to A7
     'dieresis',    'copyright',    'ordfeminine',  'guillemotleft',      // A8 to AB
     'logicalnot',  'hyphen',       'registered',   'macron',             // AC to AF
     'degree',      'plusminus',    'twosuperior',  'threesuperior',      // B0 to B3
     'acute',       'mu',           'paragraph',    'periodcentered',     // B4 to B7
     'cedilla',     'onesuperior',  'ordmasculine', 'guillemotright',     // B8 to BB
     'onequarter',  'onehalf',      'threequarters','questiondown',       // BC to BF
     'Agrave',      'Aacute',       'Acircumflex',  'Atilde',             // C0 to C3
     'Adieresis',   'Aring',        'AE',           'Ccedilla',           // C4 to C7
     'Egrave',      'Eacute',       'Ecircumflex',  'Edieresis',          // C8 to CB
     'Igrave',      'Iacute',       'Icircumflex',  'Idieresis',          // CC to CF
     'Gbreve',      'Ntilde',       'Ograve',       'Oacute',             // D0 to D3
     'Ocircumflex', 'Otilde',       'Odieresis',    'multiply',           // D4 to D7
     'Oslash',      'Ugrave',       'Uacute',       'Ucircumflex',        // D8 to DB
     'Udieresis',   'Idotaccent',   'Scedilla',     'germandbls',         // DC to DF
     'agrave',      'aacute',       'acircumflex',  'atilde',             // E0 to E3
     'adieresis',   'aring',        'ae',           'ccedilla',           // E4 to E7
     'egrave',      'eacute',       'ecircumflex',  'edieresis',          // E8 to EB
     'igrave',      'iacute',       'icircumflex',  'idieresis',          // EC to EF
     'gbreve',      'ntilde',       'ograve',       'oacute',             // F0 to F3
     'ocircumflex', 'otilde',       'odieresis',    'divide',             // F4 to F7
     'oslash',      'ugrave',       'uacute',       'ucircumflex',        // F8 to FB
     'udieresis',   'dotlessi',     'scedilla',     'ydieresis');         // FC to FF

const
  iso_8859_9_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     161,     162,     163,     164,      165,     166,     167,       // A0 to A7
     168,     169,     170,     171,     172,      173,     174,     175,       // A8 to AF
     176,     177,     178,     179,     180,      181,     182,     183,       // B0 to B7
     184,     185,     186,     187,     188,      189,     190,     191,       // B8 to BF
     192,     193,     194,     195,     196,      197,     198,     199,       // C0 to C7
     200,     201,     202,     203,     204,      205,     206,     207,       // C8 to CF
     286,     209,     210,     211,     212,      213,     214,     215,       // D0 to D7
     216,     217,     218,     219,     220,      304,     350,     223,       // D8 to DF
     224,     225,     226,     227,     228,      229,     230,     231,       // E0 to E7
     232,     233,     234,     235,     236,      237,     238,     239,       // E8 to EF
     287,     241,     242,     243,     244,      245,     246,     247,       // F0 to F7
     248,     249,     250,     251,     252,      305,     351,     255);      // F8 to FF

const
  iso_8859_11_n: array[0..255] of string =
    ('.notdef',           '.notdef',          '.notdef',        '.notdef',            // 00 to 03
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 04 to 07
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 10 to 13
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 14 to 17
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 1C to 1F
     'space',             'exclam',           'quotedbl',       'numbersign',         // 20 to 23
     'dollar',            'percent',          'ampersand',      'quotesingle',        // 24 to 27
     'parenleft',         'parenright',       'asterisk',       'plus',               // 28 to 2B
     'comma',             'hyphen',           'period',         'slash',              // 2C to 2F
     'zero',              'one',              'two',            'three',              // 30 to 33
     'four',              'five',             'six',            'seven',              // 34 to 37
     'eight',             'nine',             'colon',          'semicolon',          // 38 to 3B
     'less',              'equal',            'greater',        'question',           // 3C to 3F
     'at',                'A',                'B',              'C',                  // 40 to 43
     'D',                 'E',                'F',              'G',                  // 44 to 47
     'H',                 'I',                'J',              'K',                  // 48 to 4B
     'L',                 'M',                'N',              'O',                  // 4C to 4F
     'P',                 'Q',                'R',              'S',                  // 50 to 53
     'T',                 'U',                'V',              'W',                  // 54 to 57
     'X',                 'Y',                'Z',              'bracketleft',        // 58 to 5B
     'backslash',         'bracketright',     'asciicircum',    'underscore',         // 5C to 5F
     'grave',             'a',                'b',              'c',                  // 60 to 63
     'd',                 'e',                'f',              'g',                  // 64 to 67
     'h',                 'i',                'j',              'k',                  // 68 to 6B
     'l',                 'm',                'n',              'o',                  // 6C to 6F
     'p',                 'q',                'r',              's',                  // 70 to 73
     't',                 'u',                'v',              'w',                  // 74 to 77
     'x',                 'y',                'z',              'braceleft',          // 78 to 7B
     'bar',               'braceright',       'asciitilde',     '.notdef',            // 7C to 7F
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 80 to 83
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 84 to 87
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 88 to 8B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 8C to 8F
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 90 to 93
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 94 to 97
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 98 to 9B
     '.notdef',           '.notdef',          '.notdef',        '.notdef',            // 9C to 9F
     'space',             'kokaithai',        'khokhaithai',    'khokhuatthai',       // A0 to A3
     'khokhwaithai',      'khokhonthai',      'khorakhangthai', 'ngonguthai',         // A4 to A7
     'chochanthai',       'chochingthai',     'chochangthai',   'sosothai',           // A8 to AB
     'chochoethai',       'yoyingthai',       'dochadathai',    'topatakthai',        // AC to AF
     'thothanthai',       'thonangmonthothai','thophuthaothai', 'nonenthai',          // B0 to B3
     'dodekthai',         'totaothai',        'thothungthai',   'thothahanthai',      // B4 to B7
     'thothongthai',      'nonuthai',         'bobaimaithai',   'poplathai',          // B8 to BB
     'phophungthai',      'fofathai',         'phophanthai',    'fofanthai',          // BC to BF
     'phosamphaothai',    'momathai',         'yoyakthai',      'roruathai',          // C0 to C3
     'ruthai',            'lolingthai',       'luthai',         'wowaenthai',         // C4 to C7
     'sosalathai',        'sorusithai',       'sosuathai',      'hohipthai',          // C8 to CB
     'lochulathai',       'oangthai',         'honokhukthai',   'paiyannoithai',      // CC to CF
     'saraathai',         'maihanakatthai',   'saraaathai',     'saraamthai',         // D0 to D3
     'saraithai',         'saraiithai',       'sarauethai',     'saraueethai',        // D4 to D7
     'sarauthai',         'sarauuthai',       'phinthuthai',    '.notdef',            // D8 to DB
     '.notdef',           '.notdef',          '.notdef',        'bahtthai',           // DC to DF
     'saraethai',         'saraaethai',       'saraothai',      'saraaimaimuanthai',  // E0 to E3
     'saraaimaimalaithai','lakkhangyaothai',  'maiyamokthai',   'maitaikhuthai',      // E4 to E7
     'maiekthai',         'maithothai',       'maitrithai',     'maichattawathai',    // E8 to EB
     'thanthakhatthai',   'nikhahitthai',     'yamakkanthai',   'fongmanthai',        // EC to EF
     'zerothai',          'onethai',          'twothai',        'threethai',          // F0 to F3
     'fourthai',          'fivethai',         'sixthai',        'seventhai',          // F4 to F7
     'eightthai',         'ninethai',         'angkhankhuthai', 'khomutthai',         // F8 to FB
     '.notdef',           '.notdef',          '.notdef',        '.notdef');           // FC to FF

const
  iso_8859_11_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     3585,    3586,    3587,    3588,     3589,    3590,    3591,      // A0 to A7
     3592,    3593,    3594,    3595,    3596,     3597,    3598,    3599,      // A8 to AF
     3600,    3601,    3602,    3603,    3604,     3605,    3606,    3607,      // B0 to B7
     3608,    3609,    3610,    3611,    3612,     3613,    3614,    3615,      // B8 to BF
     3616,    3617,    3618,    3619,    3620,     3621,    3622,    3623,      // C0 to C7
     3624,    3625,    3626,    3627,    3628,     3629,    3630,    3631,      // C8 to CF
     3632,    3633,    3634,    3635,    3636,     3637,    3638,    3639,      // D0 to D7
     3640,    3641,    3642,    -1,      -1,       -1,      -1,      3647,      // D8 to DF
     3648,    3649,    3650,    3651,    3652,     3653,    3654,    3655,      // E0 to E7
     3656,    3657,    3658,    3659,    3660,     3661,    3662,    3663,      // E8 to EF
     3664,    3665,    3666,    3667,    3668,     3669,    3670,    3671,      // F0 to F7
     3672,    3673,    3674,    3675,    -1,       -1,      -1,      -1);       // F8 to FF

const
  iso_8859_15_n: array[0..255] of string =
    ('.notdef',     '.notdef',      '.notdef',      '.notdef',            // 00 to 03
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 04 to 07
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 08 to 0B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 0C to 0F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 10 to 13
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 14 to 17
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 18 to 1B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 1C to 1F
     'space',       'exclam',       'quotedbl',     'numbersign',         // 20 to 23
     'dollar',      'percent',      'ampersand',    'quotesingle',        // 24 to 27
     'parenleft',   'parenright',   'asterisk',     'plus',               // 28 to 2B
     'comma',       'hyphen',       'period',       'slash',              // 2C to 2F
     'zero',        'one',          'two',          'three',              // 30 to 33
     'four',        'five',         'six',          'seven',              // 34 to 37
     'eight',       'nine',         'colon',        'semicolon',          // 38 to 3B
     'less',        'equal',        'greater',      'question',           // 3C to 3F
     'at',          'A',            'B',            'C',                  // 40 to 43
     'D',           'E',            'F',            'G',                  // 44 to 47
     'H',           'I',            'J',            'K',                  // 48 to 4B
     'L',           'M',            'N',            'O',                  // 4C to 4F
     'P',           'Q',            'R',            'S',                  // 50 to 53
     'T',           'U',            'V',            'W',                  // 54 to 57
     'X',           'Y',            'Z',            'bracketleft',        // 58 to 5B
     'backslash',   'bracketright', 'asciicircum',  'underscore',         // 5C to 5F
     'grave',       'a',            'b',            'c',                  // 60 to 63
     'd',           'e',            'f',            'g',                  // 64 to 67
     'h',           'i',            'j',            'k',                  // 68 to 6B
     'l',           'm',            'n',            'o',                  // 6C to 6F
     'p',           'q',            'r',            's',                  // 70 to 73
     't',           'u',            'v',            'w',                  // 74 to 77
     'x',           'y',            'z',            'braceleft',          // 78 to 7B
     'bar',         'braceright',   'asciitilde',   '.notdef',            // 7C to 7F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 80 to 83
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 84 to 87
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 88 to 8B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 8C to 8F
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 90 to 93
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 94 to 97
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 98 to 9B
     '.notdef',     '.notdef',      '.notdef',      '.notdef',            // 9C to 9F
     'space',       'exclamdown',   'cent',         'sterling',           // A0 to A3
     'Euro',        'yen',          'Scaron',       'section',            // A4 to A7
     'scaron',      'copyright',    'ordfeminine',  'guillemotleft',      // A8 to AB
     'logicalnot',  'hyphen',       'registered',   'macron',             // AC to AF
     'degree',      'plusminus',    'twosuperior',  'threesuperior',      // B0 to B3
     'Zcaron',      'mu',           'paragraph',    'periodcentered',     // B4 to B7
     'zcaron',      'onesuperior',  'ordmasculine', 'guillemotright',     // B8 to BB
     'OE',          'oe',           'Ydieresis',    'questiondown',       // BC to BF
     'Agrave',      'Aacute',       'Acircumflex',  'Atilde',             // C0 to C3
     'Adieresis',   'Aring',        'AE',           'Ccedilla',           // C4 to C7
     'Egrave',      'Eacute',       'Ecircumflex',  'Edieresis',          // C8 to CB
     'Igrave',      'Iacute',       'Icircumflex',  'Idieresis',          // CC to CF
     'Eth',         'Ntilde',       'Ograve',       'Oacute',             // D0 to D3
     'Ocircumflex', 'Otilde',       'Odieresis',    'multiply',           // D4 to D7
     'Oslash',      'Ugrave',       'Uacute',       'Ucircumflex',        // D8 to DB
     'Udieresis',   'Yacute',       'Thorn',        'germandbls',         // DC to DF
     'agrave',      'aacute',       'acircumflex',  'atilde',             // E0 to E3
     'adieresis',   'aring',        'ae',           'ccedilla',           // E4 to E7
     'egrave',      'eacute',       'ecircumflex',  'edieresis',          // E8 to EB
     'igrave',      'iacute',       'icircumflex',  'idieresis',          // EC to EF
     'eth',         'ntilde',       'ograve',       'oacute',             // F0 to F3
     'ocircumflex', 'otilde',       'odieresis',    'divide',             // F4 to F7
     'oslash',      'ugrave',       'uacute',       'ucircumflex',        // F8 to FB
     'udieresis',   'yacute',       'thorn',        'ydieresis');         // FC to FF

const
  iso_8859_15_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     161,     162,     163,     8364,     165,     352,     167,       // A0 to A7
     353,     169,     170,     171,     172,      173,     174,     175,       // A8 to AF
     176,     177,     178,     179,     381,      181,     182,     183,       // B0 to B7
     382,     185,     186,     187,     338,      339,     376,     191,       // B8 to BF
     192,     193,     194,     195,     196,      197,     198,     199,       // C0 to C7
     200,     201,     202,     203,     204,      205,     206,     207,       // C8 to CF
     208,     209,     210,     211,     212,      213,     214,     215,       // D0 to D7
     216,     217,     218,     219,     220,      221,     222,     223,       // D8 to DF
     224,     225,     226,     227,     228,      229,     230,     231,       // E0 to E7
     232,     233,     234,     235,     236,      237,     238,     239,       // E8 to EF
     240,     241,     242,     243,     244,      245,     246,     247,       // F0 to F7
     248,     249,     250,     251,     252,      253,     254,     255);      // F8 to FF

const
  iso_8859_16_n: array[0..255] of string =
    ('.notdef',       '.notdef',      '.notdef',      '.notdef',            // 00 to 03
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 04 to 07
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 08 to 0B
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 0C to 0F
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 10 to 13
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 14 to 17
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 18 to 1B
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 1C to 1F
     'space',         'exclam',       'quotedbl',     'numbersign',         // 20 to 23
     'dollar',        'percent',      'ampersand',    'quotesingle',        // 24 to 27
     'parenleft',     'parenright',   'asterisk',     'plus',               // 28 to 2B
     'comma',         'hyphen',       'period',       'slash',              // 2C to 2F
     'zero',          'one',          'two',          'three',              // 30 to 33
     'four',          'five',         'six',          'seven',              // 34 to 37
     'eight',         'nine',         'colon',        'semicolon',          // 38 to 3B
     'less',          'equal',        'greater',      'question',           // 3C to 3F
     'at',            'A',            'B',            'C',                  // 40 to 43
     'D',             'E',            'F',            'G',                  // 44 to 47
     'H',             'I',            'J',            'K',                  // 48 to 4B
     'L',             'M',            'N',            'O',                  // 4C to 4F
     'P',             'Q',            'R',            'S',                  // 50 to 53
     'T',             'U',            'V',            'W',                  // 54 to 57
     'X',             'Y',            'Z',            'bracketleft',        // 58 to 5B
     'backslash',     'bracketright', 'asciicircum',  'underscore',         // 5C to 5F
     'grave',         'a',            'b',            'c',                  // 60 to 63
     'd',             'e',            'f',            'g',                  // 64 to 67
     'h',             'i',            'j',            'k',                  // 68 to 6B
     'l',             'm',            'n',            'o',                  // 6C to 6F
     'p',             'q',            'r',            's',                  // 70 to 73
     't',             'u',            'v',            'w',                  // 74 to 77
     'x',             'y',            'z',            'braceleft',          // 78 to 7B
     'bar',           'braceright',   'asciitilde',   '.notdef',            // 7C to 7F
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 80 to 83
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 84 to 87
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 88 to 8B
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 8C to 8F
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 90 to 93
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 94 to 97
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 98 to 9B
     '.notdef',       '.notdef',      '.notdef',      '.notdef',            // 9C to 9F
     'space',         'Aogonek',      'aogonek',      'Lslash',             // A0 to A3
     'Euro',          'quotedblbase', 'Scaron',       'section',            // A4 to A7
     'scaron',        'copyright',    'Scommaaccent', 'guillemotleft',      // A8 to AB
     'Zacute',        'hyphen',       'zacute',       'Zdotaccent',         // AC to AF
     'degree',        'plusminus',    'Ccaron',       'lslash',             // B0 to B3
     'Zcaron',        'quotedblright','paragraph',    'periodcentered',     // B4 to B7
     'zcaron',        'ccaron',       'scommaaccent', 'guillemotright',     // B8 to BB
     'OE',            'oe',           'Ydieresis',    'zdotaccent',         // BC to BF
     'Agrave',        'Aacute',       'Acircumflex',  'Abreve',             // C0 to C3
     'Adieresis',     'Cacute',       'AE',           'Ccedilla',           // C4 to C7
     'Egrave',        'Eacute',       'Ecircumflex',  'Edieresis',          // C8 to CB
     'Igrave',        'Iacute',       'Icircumflex',  'Idieresis',          // CC to CF
     'Dcroat',        'Nacute',       'Ograve',       'Oacute',             // D0 to D3
     'Ocircumflex',   'Ohungarumlaut','Odieresis',    'Sacute',             // D4 to D7
     'Uhungarumlaut', 'Ugrave',       'Uacute',       'Ucircumflex',        // D8 to DB
     'Udieresis',     'Eogonek',      'Tcommaaccent', 'germandbls',         // DC to DF
     'agrave',        'aacute',       'acircumflex',  'abreve',             // E0 to E3
     'adieresis',     'cacute',       'ae',           'ccedilla',           // E4 to E7
     'egrave',        'eacute',       'ecircumflex',  'edieresis',          // E8 to EB
     'igrave',        'iacute',       'icircumflex',  'idieresis',          // EC to EF
     'dcroat',        'nacute',       'ograve',       'oacute',             // F0 to F3
     'ocircumflex',   'ohungarumlaut','odieresis',    'sacute',             // F4 to F7
     'uhungarumlaut', 'ugrave',       'uacute',       'ucircumflex',        // F8 to FB
     'udieresis',     'eogonek',      'tcommaaccent', 'ydieresis');         // FC to FF

const
  iso_8859_16_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 80 to 87
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 88 to 8F
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 90 to 97
     -1,      -1,      -1,      -1,      -1,       -1,      -1,      -1,        // 98 to 9F
     160,     260,     261,     321,     8364,     8222,    352,     167,       // A0 to A7
     353,     169,     536,     171,     377,      173,     378,     379,       // A8 to AF
     176,     177,     268,     322,     381,      8221,    182,     183,       // B0 to B7
     382,     269,     537,     187,     338,      339,     376,     380,       // B8 to BF
     192,     193,     194,     258,     196,      262,     198,     199,       // C0 to C7
     200,     201,     202,     203,     204,      205,     206,     207,       // C8 to CF
     272,     323,     210,     211,     212,      336,     214,     346,       // D0 to D7
     368,     217,     218,     219,     220,      280,     538,     223,       // D8 to DF
     224,     225,     226,     259,     228,      263,     230,     231,       // E0 to E7
     232,     233,     234,     235,     236,      237,     238,     239,       // E8 to EF
     273,     324,     242,     243,     244,      337,     246,     347,       // F0 to F7
     369,     249,     250,     251,     252,      281,     539,     255);      // F8 to FF

const
  koi8_r_n: array[0..255] of string =
    ('.notdef',   '.notdef',        '.notdef',        '.notdef',            // 00 to 03
     '.notdef',   '.notdef',        '.notdef',        '.notdef',            // 04 to 07
     '.notdef',   '.notdef',        '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',   '.notdef',        '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',   '.notdef',        '.notdef',        '.notdef',            // 10 to 13
     '.notdef',   '.notdef',        '.notdef',        '.notdef',            // 14 to 17
     '.notdef',   '.notdef',        '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',   '.notdef',        '.notdef',        '.notdef',            // 1C to 1F
     'space',     'exclam',         'quotedbl',       'numbersign',         // 20 to 23
     'dollar',    'percent',        'ampersand',      'quotesingle',        // 24 to 27
     'parenleft', 'parenright',     'asterisk',       'plus',               // 28 to 2B
     'comma',     'hyphen',         'period',         'slash',              // 2C to 2F
     'zero',      'one',            'two',            'three',              // 30 to 33
     'four',      'five',           'six',            'seven',              // 34 to 37
     'eight',     'nine',           'colon',          'semicolon',          // 38 to 3B
     'less',      'equal',          'greater',        'question',           // 3C to 3F
     'at',        'A',              'B',              'C',                  // 40 to 43
     'D',         'E',              'F',              'G',                  // 44 to 47
     'H',         'I',              'J',              'K',                  // 48 to 4B
     'L',         'M',              'N',              'O',                  // 4C to 4F
     'P',         'Q',              'R',              'S',                  // 50 to 53
     'T',         'U',              'V',              'W',                  // 54 to 57
     'X',         'Y',              'Z',              'bracketleft',        // 58 to 5B
     'backslash', 'bracketright',   'asciicircum',    'underscore',         // 5C to 5F
     'grave',     'a',              'b',              'c',                  // 60 to 63
     'd',         'e',              'f',              'g',                  // 64 to 67
     'h',         'i',              'j',              'k',                  // 68 to 6B
     'l',         'm',              'n',              'o',                  // 6C to 6F
     'p',         'q',              'r',              's',                  // 70 to 73
     't',         'u',              'v',              'w',                  // 74 to 77
     'x',         'y',              'z',              'braceleft',          // 78 to 7B
     'bar',       'braceright',     'asciitilde',     '.notdef',            // 7C to 7F
     'SF100000',  'SF110000',       'SF010000',       'SF030000',           // 80 to 83
     'SF020000',  'SF040000',       'SF080000',       'SF090000',           // 84 to 87
     'SF060000',  'SF070000',       'SF050000',       'upblock',            // 88 to 8B
     'dnblock',   'block',          'lfblock',        'rtblock',            // 8C to 8F
     'ltshade',   'shade',          'dkshade',        'integraltp',         // 90 to 93
     'filledbox', 'periodcentered', 'radical',        'approxequal',        // 94 to 97
     'lessequal', 'greaterequal',   'space',          'integralbt',         // 98 to 9B
     'degree',    'twosuperior',    'periodcentered', 'divide',             // 9C to 9F
     'SF430000',  'SF240000',       'SF510000',       'afii10071',          // A0 to A3
     'SF520000',  'SF390000',       'SF220000',       'SF210000',           // A4 to A7
     'SF250000',  'SF500000',       'SF490000',       'SF380000',           // A8 to AB
     'SF280000',  'SF270000',       'SF260000',       'SF360000',           // AC to AF
     'SF370000',  'SF420000',       'SF190000',       'afii10023',          // B0 to B3
     'SF200000',  'SF230000',       'SF470000',       'SF480000',           // B4 to B7
     'SF410000',  'SF450000',       'SF460000',       'SF400000',           // B8 to BB
     'SF540000',  'SF530000',       'SF440000',       'copyright',          // BC to BF
     'afii10096', 'afii10065',      'afii10066',      'afii10088',          // C0 to C3
     'afii10069', 'afii10070',      'afii10086',      'afii10068',          // C4 to C7
     'afii10087', 'afii10074',      'afii10075',      'afii10076',          // C8 to CB
     'afii10077', 'afii10078',      'afii10079',      'afii10080',          // CC to CF
     'afii10081', 'afii10097',      'afii10082',      'afii10083',          // D0 to D3
     'afii10084', 'afii10085',      'afii10072',      'afii10067',          // D4 to D7
     'afii10094', 'afii10093',      'afii10073',      'afii10090',          // D8 to DB
     'afii10095', 'afii10091',      'afii10089',      'afii10092',          // DC to DF
     'afii10048', 'afii10017',      'afii10018',      'afii10040',          // E0 to E3
     'afii10021', 'afii10022',      'afii10038',      'afii10020',          // E4 to E7
     'afii10039', 'afii10026',      'afii10027',      'afii10028',          // E8 to EB
     'afii10029', 'afii10030',      'afii10031',      'afii10032',          // EC to EF
     'afii10033', 'afii10049',      'afii10034',      'afii10035',          // F0 to F3
     'afii10036', 'afii10037',      'afii10024',      'afii10019',          // F4 to F7
     'afii10046', 'afii10045',      'afii10025',      'afii10042',          // F8 to FB
     'afii10047', 'afii10043',      'afii10041',      'afii10044');         // FC to FF

const
  koi8_r_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     9472,    9474,    9484,    9488,    9492,     9496,    9500,    9508,      // 80 to 87
     9516,    9524,    9532,    9600,    9604,     9608,    9612,    9616,      // 88 to 8F
     9617,    9618,    9619,    8992,    9632,     8729,    8730,    8776,      // 90 to 97
     8804,    8805,    160,     8993,    176,      178,     183,     247,       // 98 to 9F
     9552,    9553,    9554,    1105,    9555,     9556,    9557,    9558,      // A0 to A7
     9559,    9560,    9561,    9562,    9563,     9564,    9565,    9566,      // A8 to AF
     9567,    9568,    9569,    1025,    9570,     9571,    9572,    9573,      // B0 to B7
     9574,    9575,    9576,    9577,    9578,     9579,    9580,    169,       // B8 to BF
     1102,    1072,    1073,    1094,    1076,     1077,    1092,    1075,      // C0 to C7
     1093,    1080,    1081,    1082,    1083,     1084,    1085,    1086,      // C8 to CF
     1087,    1103,    1088,    1089,    1090,     1091,    1078,    1074,      // D0 to D7
     100,     1099,    1079,    1096,    1101,     1097,    1095,    1098,      // D8 to DF
     1070,    1040,    1041,    1062,    1044,     1045,    1060,    1043,      // E0 to E7
     1061,    1048,    1049,    1050,    1051,     1052,    1053,    1054,      // E8 to EF
     1055,    1071,    1056,    1057,    1058,     1059,    1046,    1042,      // F0 to F7
     1068,    1067,    1047,    1064,    1069,     1065,    1063,    1066);     // F8 to FF

const
  koi8_u_n: array[0..255] of string =
    ('.notdef',   '.notdef',      '.notdef',        '.notdef',            // 00 to 03
     '.notdef',   '.notdef',      '.notdef',        '.notdef',            // 04 to 07
     '.notdef',   '.notdef',      '.notdef',        '.notdef',            // 08 to 0B
     '.notdef',   '.notdef',      '.notdef',        '.notdef',            // 0C to 0F
     '.notdef',   '.notdef',      '.notdef',        '.notdef',            // 10 to 13
     '.notdef',   '.notdef',      '.notdef',        '.notdef',            // 14 to 17
     '.notdef',   '.notdef',      '.notdef',        '.notdef',            // 18 to 1B
     '.notdef',   '.notdef',      '.notdef',        '.notdef',            // 1C to 1F
     'space',     'exclam',       'quotedbl',       'numbersign',         // 20 to 23
     'dollar',    'percent',      'ampersand',      'quotesingle',        // 24 to 27
     'parenleft', 'parenright',   'asterisk',       'plus',               // 28 to 2B
     'comma',     'hyphen',       'period',         'slash',              // 2C to 2F
     'zero',      'one',          'two',            'three',              // 30 to 33
     'four',      'five',         'six',            'seven',              // 34 to 37
     'eight',     'nine',         'colon',          'semicolon',          // 38 to 3B
     'less',      'equal',        'greater',        'question',           // 3C to 3F
     'at',        'A',            'B',              'C',                  // 40 to 43
     'D',         'E',            'F',              'G',                  // 44 to 47
     'H',         'I',            'J',              'K',                  // 48 to 4B
     'L',         'M',            'N',              'O',                  // 4C to 4F
     'P',         'Q',            'R',              'S',                  // 50 to 53
     'T',         'U',            'V',              'W',                  // 54 to 57
     'X',         'Y',            'Z',              'bracketleft',        // 58 to 5B
     'backslash', 'bracketright', 'asciicircum',    'underscore',         // 5C to 5F
     'grave',     'a',            'b',              'c',                  // 60 to 63
     'd',         'e',            'f',              'g',                  // 64 to 67
     'h',         'i',            'j',              'k',                  // 68 to 6B
     'l',         'm',            'n',              'o',                  // 6C to 6F
     'p',         'q',            'r',              's',                  // 70 to 73
     't',         'u',            'v',              'w',                  // 74 to 77
     'x',         'y',            'z',              'braceleft',          // 78 to 7B
     'bar',       'braceright',   'asciitilde',     '.notdef',            // 7C to 7F
     'SF100000',  'SF110000',     'SF010000',       'SF030000',           // 80 to 83
     'SF020000',  'SF040000',     'SF080000',       'SF090000',           // 84 to 87
     'SF060000',  'SF070000',     'SF050000',       'upblock',            // 88 to 8B
     'dnblock',   'block',        'lfblock',        'rtblock',            // 8C to 8F
     'ltshade',   'shade',        'dkshade',        'integraltp',         // 90 to 93
     'filledbox', 'bullet',       'radical',        'approxequal',        // 94 to 97
     'lessequal', 'greaterequal', 'space',          'integralbt',         // 98 to 9B
     'degree',    'twosuperior',  'periodcentered', 'divide',             // 9C to 9F
     'SF430000',  'SF240000',     'SF510000',       'afii10071',          // A0 to A3
     'afii10101', 'SF390000',     'afii10103',      'afii10104',          // A4 to A7
     'SF250000',  'SF500000',     'SF490000',       'SF380000',           // A8 to AB
     'SF280000',  'afii10098',    'SF260000',       'SF360000',           // AC to AF
     'SF370000',  'SF420000',     'SF190000',       'afii10023',          // B0 to B3
     'afii10053', 'SF230000',     'afii10055',      'afii10056',          // B4 to B7
     'SF410000',  'SF450000',     'SF460000',       'SF400000',           // B8 to BB
     'SF540000',  'afii10050',    'SF440000',       'copyright',          // BC to BF
     'afii10096', 'afii10065',    'afii10066',      'afii10088',          // C0 to C3
     'afii10069', 'afii10070',    'afii10086',      'afii10068',          // C4 to C7
     'afii10087', 'afii10074',    'afii10075',      'afii10076',          // C8 to CB
     'afii10077', 'afii10078',    'afii10079',      'afii10080',          // CC to CF
     'afii10081', 'afii10097',    'afii10082',      'afii10083',          // D0 to D3
     'afii10084', 'afii10085',    'afii10072',      'afii10067',          // D4 to D7
     'afii10094', 'afii10093',    'afii10073',      'afii10090',          // D8 to DB
     'afii10095', 'afii10091',    'afii10089',      'afii10092',          // DC to DF
     'afii10048', 'afii10017',    'afii10018',      'afii10040',          // E0 to E3
     'afii10021', 'afii10022',    'afii10038',      'afii10020',          // E4 to E7
     'afii10039', 'afii10026',    'afii10027',      'afii10028',          // E8 to EB
     'afii10029', 'afii10030',    'afii10031',      'afii10032',          // EC to EF
     'afii10033', 'afii10049',    'afii10034',      'afii10035',          // F0 to F3
     'afii10036', 'afii10037',    'afii10024',      'afii10019',          // F4 to F7
     'afii10046', 'afii10045',    'afii10025',      'afii10042',          // F8 to FB
     'afii10047', 'afii10043',    'afii10041',      'afii10044');         // FC to FF

const
  koi8_u_v: array[0..255] of Word =
    (0,       1,       2,       3,       4,        5,       6,       7,         // 00 to 07
     8,       9,       10,      11,      12,       13,      14,      15,        // 08 to 0F
     16,      17,      18,      19,      20,       21,      22,      23,        // 10 to 17
     24,      25,      26,      27,      28,       29,      30,      31,        // 18 to 1F
     32,      33,      34,      35,      36,       37,      38,      39,        // 20 to 27
     40,      41,      42,      43,      44,       45,      46,      47,        // 28 to 2F
     48,      49,      50,      51,      52,       53,      54,      55,        // 30 to 37
     56,      57,      58,      59,      60,       61,      62,      63,        // 38 to 3F
     64,      65,      66,      67,      68,       69,      70,      71,        // 40 to 47
     72,      73,      74,      75,      76,       77,      78,      79,        // 48 to 4F
     80,      81,      82,      83,      84,       85,      86,      87,        // 50 to 57
     88,      89,      90,      91,      92,       93,      94,      95,        // 58 to 5F
     96,      97,      98,      99,      100,      101,     102,     103,       // 60 to 67
     104,     105,     106,     107,     108,      109,     110,     111,       // 68 to 6F
     112,     113,     114,     115,     116,      117,     118,     119,       // 70 to 77
     120,     121,     122,     123,     124,      125,     126,     127,       // 78 to 7F
     9472,    9474,    9484,    9488,    9492,     9496,    9500,    9508,      // 80 to 87
     9516,    9524,    9532,    9600,    9604,     9608,    9612,    9616,      // 88 to 8F
     9617,    9618,    9619,    8992,    9632,     8226,    8730,    8776,      // 90 to 97
     8804,    8805,    160,     8993,    176,      178,     183,     247,       // 98 to 9F
     9552,    9553,    9554,    1105,    1108,     9556,    1110,    1111,      // A0 to A7
     9559,    9560,    9561,    9562,    9563,     1169,    9565,    9566,      // A8 to AF
     9567,    9568,    9569,    1025,    1028,     9571,    1030,    1031,      // B0 to B7
     9574,    9575,    9576,    9577,    9578,     1168,    9580,    169,       // B8 to BF
     1102,    1072,    1073,    1094,    1076,     1077,    1092,    1075,      // C0 to C7
     1093,    1080,    1081,    1082,    1083,     1084,    1085,    1086,      // C8 to CF
     1087,    1103,    1088,    1089,    1090,     1091,    1078,    1074,      // D0 to D7
     1100,    1099,    1079,    1096,    1101,     1097,    1095,    1098,      // D8 to DF
     1070,    1040,    1041,    1062,    1044,     1045,    1060,    1043,      // E0 to E7
     1061,    1048,    1049,    1050,    1051,     1052,    1053,    1054,      // E8 to EF
     1055,    1071,    1056,    1057,    1058,     1059,    1046,    1042,      // F0 to F7
     1068,    1067,    1047,    1064,    1069,     1065,    1063,    1066);     // F8 to FF

implementation

end.

