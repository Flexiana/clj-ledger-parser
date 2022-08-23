journal:
    journal_item journal |
    epsilon
    ;

journal_item:
    whitespace
    directive |
    xact |
    epsilon
    ;

whitespace:
    EOL |
    WHITESPACE EOL |
    ';' TEXT EOL |              
    '*' TEXT EOL |
    epsilon
    ;

directive:
    '@' word_directive EOL |
    '!' word_directive EOL |
    word_directive EOL |
    char_directive EOL
    ;

word_directive:
    "include" TEXT |
    "account" TEXT |
    "end" |
    "alias" STRING '=' TEXT |
    "def" TEXT |
    TEXT WHITESPACE TEXT        
    ;

char_directive:
    'i' date time TEXT |        
    'I' date time TEXT |
    'o' date time TEXT |        
    'O' date time TEXT |
    'h' TEXT EOL |
    'b' TEXT EOL |
    'D' amount |                
    'A' TEXT |                  
    'C' commodity '=' amount |  
    'P' date time commodity amount | 
    'N' commodity |             
    'Y' INT4 |                  
    '-' '-' STRING TEXT |
    epsilon
    ;

date: INT4 date_sep INT2 date_sep INT2 ;
date_opt: '=' date | epsilon ;
date_sep: '/' | '-' | '.' ;

time: INT2 ':' INT2 ':' INT2 ;

commodity:
    '"' TEXT '"' |
    STRING ;

xact: plain_xact |
       periodic_xact |
       automated_xact ;

plain_xact:
    date date_opt status_opt code_opt FULLSTRING note_opt EOL
    postings ;

status_opt: status | epsilon ;
status: '*' | '!' | epsilon ;

code_opt: code | epsilon ;
code: '(' TEXT ')' ;

spacer: ' ' ' ' | '\t' | ' ' '\t' ;

note_opt: spacer note | epsilon ;
note: ';' TEXT ;



periodic_xact:
    '~' period_expr note_opt EOL
    posting postings ;

period_expr: FULLSTRING ;



automated_xact:
    '=' value_expr note_opt EOL
    posting postings ;

value_expr: FULLSTRING ;

quantity: neg_opt BIGINT decimal_opt ;

neg_opt: '-' | epsilon ;
decimal_opt: '.' BIGINT | epsilon ;

annotation: lot_price_opt lot_date_opt lot_note_opt ;

lot_date_opt: date | epsilon ;
lot_date: '[' date ']' ;

lot_price_opt: price | epsilon ;
lot_price: '{' amount '}' ;

lot_note_opt: note | epsilon ;
lot_note: '(' string ')' ;

amount:
    neg_opt commodity quantity annotation |
    quantity commodity annotation ;

amount_expr: amount | value_expr ;

postings:
    posting postings |
    epsilon
    ;

posting:
    WHITESPACE status_opt account values_opt note_opt EOL;

account_name: FULLSTRING ;

values_opt:
    spacer amount_expr price_opt |
    epsilon
    ;

price_opt: price | epsilon ;
price:
    '@' amount_expr |
    '@@' amount_expr            
    ;

account:
    account_name |
    '(' account_name ')' |
    '[' account_name ']' ;

INT1: #"\d";
INT2: #"\d{2}";
INT4: #"\d{4}";
BIGINT: #"\d+";

TEXT: #".*[^\n]";

WHITESPACE: #"\s+";

STRING: #"[\w]+[^\s\n]";
FULLSTRING: #".*[^\n]";
