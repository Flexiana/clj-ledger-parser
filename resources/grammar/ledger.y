<journal>:
    journal_item journal |
    epsilon
    ;

journal_item:
    whitespace directive |
    xact |
    epsilon
    ;

whitespace:
    <eol> |
    horizontal_whitespace <eol> |
    ';' text <eol> |
    '*' text <eol> |
    epsilon
    ;

directive:
    '@' word_directive eol |
    '!' word_directive eol |
    word_directive eol |
    char_directive eol
    ;

word_directive:
    'include' text |
    'account' text |
    'end' |
    'alias' string '=' text |
    'def' text |
    text horizontal_whitespace text
    ;

char_directive:
    'i' date time text |
    'I' date time text |
    'o' date time text |
    'O' date time text |
    'h' text eol |
    'b' text eol |
    'D' amount |
    'A' text |
    'C' commodity '=' amount |
    'P' date time commodity amount |
    'N' commodity |
    'Y' int4 |
    '-' '-' string text |
    epsilon
    ;

date: int4 date_sep int2 date_sep int2 ;
date_opt: '=' date | epsilon ;
date_sep: '/' | '-' | '.' ;

time: int2 ':' int2 ':' int2 ;

commodity:
    '"' text '"' |
    string ;

xact: plain_xact |
       periodic_xact |
       automated_xact ;

plain_xact:
    date date_opt status_opt code_opt fullstring note_opt eol
    postings ;

status_opt: status | epsilon ;
status: '*' | '!' | epsilon ;

code_opt: code | epsilon ;
code: '(' text ')' ;

spacer: ' ' ' ' | '\t' | ' ' '\t' ;

note_opt: spacer note | epsilon ;
note: ';' text ;

periodic_xact:
    '~' period_expr note_opt eol
    posting postings ;

period_expr: fullstring ;

automated_xact:
    '=' value_expr note_opt eol
    posting postings ;

value_expr: fullstring ;

quantity: neg_opt bigint decimal_opt ;

neg_opt: '-' | epsilon ;
decimal_opt: '.' bigint | epsilon ;

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
    horizontal_whitespace status_opt account values_opt note_opt eol ;

account_name: fullstring ;

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

<eol>: <#"\n|$"> ;

int1: #"\d" ;
int2: #"\d{2}" ;
int4: #"\d{4}" ;
bigint: #"\d+" ;

text: #"\S?[^\n]*\S" ;

horizontal_whitespace: #"[ \t]"+ ;

string: #"\w+[^\s]" ;
fullstring: #".*[^\n]" ;
