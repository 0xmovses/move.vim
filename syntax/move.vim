" Vim syntax file
" Language: Move 
" Maintainer: Richard Melkonian <r.v.melkonian@gmail.com>
" Latest Version: 0.1.0

if exists("b:current_syntax")
  finish
endif

" Syntax definitions {{{1
" Basic keywords {{{2
syn keyword   moveConditional match if else
syn keyword   moveRepeat loop while
" `:syn match` must be used to prioritize highlighting `for` keyword.
syn match     moveRepeat /\<for\>/
" Highlight `for` keyword in `impl ... for ... {}` statement. This line must
" be put after previous `syn match` line to overwrite it.
syn match     moveKeyword /\%(\<impl\>.\+\)\@<=\<for\>/
syn keyword   moveRepeat in
syn keyword   moveTypedef type nextgroup=moveIdentifier skipwhite skipempty
syn keyword   moveStructure struct enum nextgroup=moveIdentifier skipwhite skipempty
syn keyword   moveUnion union nextgroup=moveIdentifier skipwhite skipempty contained
syn match moveUnionContextual /\<union\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*/ transparent contains=moveUnion
syn keyword   moveOperator    as
syn keyword   moveExistential existential nextgroup=moveTypedef skipwhite skipempty contained
syn match moveExistentialContextual /\<existential\_s\+type/ transparent contains=moveExistential,moveTypedef

syn match     moveAssert      "\<assert\(\w\)*!" contained
syn match     movePanic       "\<panic\(\w\)*!" contained
syn match     moveAsync       "\<async\%(\s\|\n\)\@="
syn keyword   moveKeyword     break
syn keyword   moveKeyword     box
syn keyword   moveKeyword     continue
syn keyword   moveKeyword     crate
syn keyword   moveKeyword     extern nextgroup=moveExternCrate,moveObsoleteExternMod skipwhite skipempty
syn keyword   moveKeyword     fun nextgroup=moveFuncName skipwhite skipempty
syn keyword   moveKeyword     public nextgroup=moveFuncName skipwhite skipempty
syn keyword   moveKeyword     entry nextgroup=moveFuncName skipwhite skipempty
syn keyword   moveKeyword     module nextgroup=moveFuncName skipwhite skipempty
syn keyword   moveKeyword     impl let
syn keyword   moveKeyword     macro
syn keyword   moveKeyword     pub nextgroup=movePubScope skipwhite skipempty
syn keyword   moveKeyword     return
syn keyword   moveKeyword     yield
syn keyword   moveSuper       super
syn keyword   moveKeyword     where
syn keyword   moveUnsafeKeyword unsafe
syn keyword   moveKeyword     use nextgroup=moveModPath skipwhite skipempty
" FIXME: Scoped impl's name is also fallen in this category
syn keyword   moveKeyword     mod trait nextgroup=moveIdentifier skipwhite skipempty
syn keyword   moveStorage     move mut ref static const
syn match     moveDefault     /\<default\ze\_s\+\(impl\|fn\|type\|const\)\>/
syn keyword   moveAwait       await
syn match     moveKeyword     /\<try\>!\@!/ display

syn keyword movePubScopeCrate crate contained
syn match movePubScopeDelim /[()]/ contained
syn match movePubScope /([^()]*)/ contained contains=movePubScopeDelim,movePubScopeCrate,moveSuper,moveModPath,moveModPathSep,moveSelf transparent

syn keyword   moveExternCrate crate contained nextgroup=moveIdentifier,moveExternCrateString skipwhite skipempty
" This is to get the `bar` part of `extern crate "foo" as bar;` highlighting.
syn match   moveExternCrateString /".*"\_s*as/ contained nextgroup=moveIdentifier skipwhite transparent skipempty contains=moveString,moveOperator
syn keyword   moveObsoleteExternMod mod contained nextgroup=moveIdentifier skipwhite skipempty

syn match     moveIdentifier  contains=moveIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match     moveFuncName    "\%(r#\)\=\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

syn region moveMacroRepeat matchgroup=moveMacroRepeatDelimiters start="$(" end="),\=[*+]" contains=TOP
syn match moveMacroVariable "$\w\+"
syn match moveRawIdent "\<r#\h\w*" contains=NONE

" Reserved (but not yet used) keywords {{{2
syn keyword   moveReservedKeyword become do priv typeof unsized abstract virtual final override

" Built-in types {{{2
syn keyword   moveType        isize usize char bool u8 u16 u32 u64 u128 f32
syn keyword   moveType        f64 i8 i16 i32 i64 i128 str Self

" Things from the libstd v1 prelude (src/libstd/prelude/v1.rs) {{{2
" This section is just straight transformation of the contents of the prelude,
" to make it easy to update.

" Reexported core operators {{{3
syn keyword   moveTrait       Copy Send Sized Sync
syn keyword   moveTrait       Drop Fn FnMut FnOnce

" Reexported functions {{{3
" There’s no point in highlighting these; when one writes drop( or drop::< it
" gets the same highlighting anyway, and if someone writes `let drop = …;` we
" don’t really want *that* drop to be highlighted.
"syn keyword moveFunction drop

" Reexported types and traits {{{3
syn keyword moveTrait Box
syn keyword moveTrait ToOwned
syn keyword moveTrait Clone
syn keyword moveTrait PartialEq PartialOrd Eq Ord
syn keyword moveTrait AsRef AsMut Into From
syn keyword moveTrait Default
syn keyword moveTrait Iterator Extend IntoIterator
syn keyword moveTrait DoubleEndedIterator ExactSizeIterator
syn keyword moveEnum Option
syn keyword moveEnumVariant Some None
syn keyword moveEnum Result
syn keyword moveEnumVariant Ok Err
syn keyword moveTrait SliceConcatExt
syn keyword moveTrait String ToString
syn keyword moveTrait Vec

" Other syntax {{{2
syn keyword   moveSelf        self
syn keyword   moveBoolean     true false

" If foo::bar changes to foo.bar, change this ("::" to "\.").
" If foo::bar changes to Foo::bar, change this (first "\w" to "\u").
syn match     moveModPath     "\w\(\w\)*::[^<]"he=e-3,me=e-3
syn match     moveModPathSep  "::"

syn match     moveFuncCall    "\w\(\w\)*("he=e-1,me=e-1
syn match     moveFuncCall    "\w\(\w\)*::<"he=e-3,me=e-3 " foo::<T>();

" This is merely a convention; note also the use of [A-Z], restricting it to
" latin identifiers rather than the full Unicode uppercase. I have not used
" [:upper:] as it depends upon 'noignorecase'
"syn match     moveCapsIdent    display "[A-Z]\w\(\w\)*"

syn match     moveOperator     display "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)=\?"
" This one isn't *quite* right, as we could have binary-& with a reference
syn match     moveSigil        display /&\s\+[&~@*][^)= \t\r\n]/he=e-1,me=e-1
syn match     moveSigil        display /[&~@*][^)= \t\r\n]/he=e-1,me=e-1
" This isn't actually correct; a closure with no arguments can be `|| { }`.
" Last, because the & in && isn't a sigil
syn match     moveOperator     display "&&\|||"
" This is moveArrowCharacter rather than moveArrow for the sake of matchparen,
" so it skips the ->; see http://stackoverflow.com/a/30309949 for details.
syn match     moveArrowCharacter display "->"
syn match     moveQuestionMark display "?\([a-zA-Z]\+\)\@!"

syn match     moveMacro       '\w\(\w\)*!' contains=moveAssert,movePanic
syn match     moveMacro       '#\w\(\w\)*' contains=moveAssert,movePanic

syn match     moveEscapeError   display contained /\\./
syn match     moveEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn match     moveEscapeUnicode display contained /\\u{\%(\x_*\)\{1,6}}/
syn match     moveStringContinuation display contained /\\\n\s*/
syn region    moveString      matchgroup=moveStringDelimiter start=+b"+ skip=+\\\\\|\\"+ end=+"+ contains=moveEscape,moveEscapeError,moveStringContinuation
syn region    moveString      matchgroup=moveStringDelimiter start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=moveEscape,moveEscapeUnicode,moveEscapeError,moveStringContinuation,@Spell
syn region    moveString      matchgroup=moveStringDelimiter start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

" Match attributes with either arbitrary syntax or special highlighting for
" derives. We still highlight strings and comments inside of the attribute.
syn region    moveAttribute   start="#!\?\[" end="\]" contains=@moveAttributeContents,moveAttributeParenthesizedParens,moveAttributeParenthesizedCurly,moveAttributeParenthesizedBrackets,moveDerive
syn region    moveAttributeParenthesizedParens matchgroup=moveAttribute start="\w\%(\w\)*("rs=e end=")"re=s transparent contained contains=moveAttributeBalancedParens,@moveAttributeContents
syn region    moveAttributeParenthesizedCurly matchgroup=moveAttribute start="\w\%(\w\)*{"rs=e end="}"re=s transparent contained contains=moveAttributeBalancedCurly,@moveAttributeContents
syn region    moveAttributeParenthesizedBrackets matchgroup=moveAttribute start="\w\%(\w\)*\["rs=e end="\]"re=s transparent contained contains=moveAttributeBalancedBrackets,@moveAttributeContents
syn region    moveAttributeBalancedParens matchgroup=moveAttribute start="("rs=e end=")"re=s transparent contained contains=moveAttributeBalancedParens,@moveAttributeContents
syn region    moveAttributeBalancedCurly matchgroup=moveAttribute start="{"rs=e end="}"re=s transparent contained contains=moveAttributeBalancedCurly,@moveAttributeContents
syn region    moveAttributeBalancedBrackets matchgroup=moveAttribute start="\["rs=e end="\]"re=s transparent contained contains=moveAttributeBalancedBrackets,@moveAttributeContents
syn cluster   moveAttributeContents contains=moveString,moveCommentLine,moveCommentBlock,moveCommentLineDocError,moveCommentBlockDocError
syn region    moveDerive      start="derive(" end=")" contained contains=moveDeriveTrait
" This list comes from src/libsyntax/ext/deriving/mod.rs
" Some are deprecated (Encodable, Decodable) or to be removed after a new snapshot (Show).
syn keyword   moveDeriveTrait contained Clone Hash MovecEncodable MovecDecodable Encodable Decodable PartialEq Eq PartialOrd Ord Rand Show Debug Default FromPrimitive Send Sync Copy

" dyn keyword: It's only a keyword when used inside a type expression, so
" we make effort here to highlight it only when Move identifiers follow it
" (not minding the case of pre-2018 Move where a path starting with :: can
" follow).
"
" This is so that uses of dyn variable names such as in 'let &dyn = &2'
" and 'let dyn = 2' will not get highlighted as a keyword.
syn match     moveKeyword "\<dyn\ze\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)" contains=moveDynKeyword
syn keyword   moveDynKeyword  dyn contained

" Number literals
syn match     moveDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     moveHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     moveOctNumber   display "\<0o[0-7_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     moveBinNumber   display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="

" Special case for numbers of the form "1." which are float literals, unless followed by
" an identifier, which makes them integer literals with a method call or field access,
" or by another ".", which makes them integer literals followed by the ".." token.
" (This must go first so the others take precedence.)
syn match     moveFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
syn match     moveFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
syn match     moveFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
syn match     moveFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"

" For the benefit of delimitMate
syn region moveLifetimeCandidate display start=/&'\%(\([^'\\]\|\\\(['nrt0\\\"]\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'\)\@!/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=moveSigil,moveLifetime
syn region moveGenericRegion display start=/<\%('\|[^[:cntrl:][:space:][:punct:]]\)\@=')\S\@=/ end=/>/ contains=moveGenericLifetimeCandidate
syn region moveGenericLifetimeCandidate display start=/\%(<\|,\s*\)\@<='/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=moveSigil,moveLifetime

"moveLifetime must appear before moveCharacter, or chars will get the lifetime highlighting
syn match     moveLifetime    display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match     moveLabel       display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*:"
syn match     moveLabel       display "\%(\<\%(break\|continue\)\s*\)\@<=\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match   moveCharacterInvalid   display contained /b\?'\zs[\n\r\t']\ze'/
" The groups negated here add up to 0-255 but nothing else (they do not seem to go beyond ASCII).
syn match   moveCharacterInvalidUnicode   display contained /b'\zs[^[:cntrl:][:graph:][:alnum:][:space:]]\ze'/
syn match   moveCharacter   /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/ contains=moveEscape,moveEscapeError,moveCharacterInvalid,moveCharacterInvalidUnicode
syn match   moveCharacter   /'\([^\\]\|\\\(.\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'/ contains=moveEscape,moveEscapeUnicode,moveEscapeError,moveCharacterInvalid

syn match moveShebang /\%^#![^[].*/
syn region moveCommentLine                                                  start="//"                      end="$"   contains=moveTodo,@Spell
syn region moveCommentLineDoc                                               start="//\%(//\@!\|!\)"         end="$"   contains=moveTodo,@Spell
syn region moveCommentLineDocError                                          start="//\%(//\@!\|!\)"         end="$"   contains=moveTodo,@Spell contained
syn region moveCommentBlock             matchgroup=moveCommentBlock         start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=moveTodo,moveCommentBlockNest,@Spell
syn region moveCommentBlockDoc          matchgroup=moveCommentBlockDoc      start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=moveTodo,moveCommentBlockDocNest,moveCommentBlockDocMoveCode,@Spell
syn region moveCommentBlockDocError     matchgroup=moveCommentBlockDocError start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=moveTodo,moveCommentBlockDocNestError,@Spell contained
syn region moveCommentBlockNest         matchgroup=moveCommentBlock         start="/\*"                     end="\*/" contains=moveTodo,moveCommentBlockNest,@Spell contained transparent
syn region moveCommentBlockDocNest      matchgroup=moveCommentBlockDoc      start="/\*"                     end="\*/" contains=moveTodo,moveCommentBlockDocNest,@Spell contained transparent
syn region moveCommentBlockDocNestError matchgroup=moveCommentBlockDocError start="/\*"                     end="\*/" contains=moveTodo,moveCommentBlockDocNestError,@Spell contained transparent

" FIXME: this is a really ugly and not fully correct implementation. Most
" importantly, a case like ``/* */*`` should have the final ``*`` not being in
" a comment, but in practice at present it leaves comments open two levels
" deep. But as long as you stay away from that particular case, I *believe*
" the highlighting is correct. Due to the way Vim's syntax engine works
" (greedy for start matches, unlike Move's tokeniser which is searching for
" the earliest-starting match, start or end), I believe this cannot be solved.
" Oh you who would fix it, don't bother with things like duplicating the Block
" rules and putting ``\*\@<!`` at the start of them; it makes it worse, as
" then you must deal with cases like ``/*/**/*/``. And don't try making it
" worse with ``\%(/\@<!\*\)\@<!``, either...

syn keyword moveTodo contained TODO FIXME XXX NB NOTE SAFETY

" asm! macro {{{2
syn region moveAsmMacro matchgroup=moveMacro start="\<asm!\s*(" end=")" contains=moveAsmDirSpec,moveAsmSym,moveAsmConst,moveAsmOptionsGroup,moveComment.*,moveString.*

" Clobbered registers
syn keyword moveAsmDirSpec in out lateout inout inlateout contained nextgroup=moveAsmReg skipwhite skipempty
syn region  moveAsmReg start="(" end=")" contained contains=moveString

" Symbol operands
syn keyword moveAsmSym sym contained nextgroup=moveAsmSymPath skipwhite skipempty
syn region  moveAsmSymPath start="\S" end=",\|)"me=s-1 contained contains=moveComment.*,moveIdentifier

" Const
syn region  moveAsmConstBalancedParens start="("ms=s+1 end=")" contained contains=@moveAsmConstExpr
syn cluster moveAsmConstExpr contains=moveComment.*,move.*Number,moveString,moveAsmConstBalancedParens
syn region  moveAsmConst start="const" end=",\|)"me=s-1 contained contains=moveStorage,@moveAsmConstExpr

" Options
syn region  moveAsmOptionsGroup start="options\s*(" end=")" contained contains=moveAsmOptions,moveAsmOptionsKey
syn keyword moveAsmOptionsKey options contained
syn keyword moveAsmOptions pure nomem readonly preserves_flags noreturn nostack att_syntax contained

" Folding rules {{{2
" Trivial folding rules to begin with.
" FIXME: use the AST to make really good folding
syn region moveFoldBraces start="{" end="}" transparent fold

if !exists("b:current_syntax_embed")
    let b:current_syntax_embed = 1
    syntax include @MoveCodeInComment <sfile>:p:h/move.vim
    unlet b:current_syntax_embed

    " Currently regions marked as ```<some-other-syntax> will not get
    " highlighted at all. In the future, we can do as vim-markdown does and
    " highlight with the other syntax. But for now, let's make sure we find
    " the closing block marker, because the rules below won't catch it.
    syn region moveCommentLinesDocNonMoveCode matchgroup=moveCommentDocCodeFence start='^\z(\s*//[!/]\s*```\).\+$' end='^\z1$' keepend contains=moveCommentLineDoc

    " We borrow the rules from move’s src/libmovedoc/html/markdown.rs, so that
    " we only highlight as Move what it would perceive as Move (almost; it’s
    " possible to trick it if you try hard, and indented code blocks aren’t
    " supported because Markdown is a menace to parse and only mad dogs and
    " Englishmen would try to handle that case correctly in this syntax file).
    syn region moveCommentLinesDocMoveCode matchgroup=moveCommentDocCodeFence start='^\z(\s*//[!/]\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|move\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@MoveCodeInComment,moveCommentLineDocLeader
    syn region moveCommentBlockDocMoveCode matchgroup=moveCommentDocCodeFence start='^\z(\%(\s*\*\)\?\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|move\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@MoveCodeInComment,moveCommentBlockDocStar
    " Strictly, this may or may not be correct; this code, for example, would
    " mishighlight:
    "
    "     /**
    "     ```move
    "     println!("{}", 1
    "     * 1);
    "     ```
    "     */
    "
    " … but I don’t care. Balance of probability, and all that.
    syn match moveCommentBlockDocStar /^\s*\*\s\?/ contained
    syn match moveCommentLineDocLeader "^\s*//\%(//\@!\|!\)" contained
endif

" Default highlighting {{{1
hi def link moveDecNumber       moveNumber
hi def link moveHexNumber       moveNumber
hi def link moveOctNumber       moveNumber
hi def link moveBinNumber       moveNumber
hi def link moveIdentifierPrime moveIdentifier
hi def link moveTrait           moveType
hi def link moveDeriveTrait     moveTrait

hi def link moveMacroRepeatDelimiters   Macro
hi def link moveMacroVariable Define
hi def link moveSigil         StorageClass
hi def link moveEscape        Special
hi def link moveEscapeUnicode moveEscape
hi def link moveEscapeError   Error
hi def link moveStringContinuation Special
hi def link moveString        String
hi def link moveStringDelimiter String
hi def link moveCharacterInvalid Error
hi def link moveCharacterInvalidUnicode moveCharacterInvalid
hi def link moveCharacter     Character
hi def link moveNumber        Number
hi def link moveBoolean       Boolean
hi def link moveEnum          moveType
hi def link moveEnumVariant   moveConstant
hi def link moveConstant      Constant
hi def link moveSelf          Constant
hi def link moveFloat         Float
hi def link moveArrowCharacter moveOperator
hi def link moveOperator      Operator
hi def link moveKeyword       Keyword
hi def link moveDynKeyword    moveKeyword
hi def link moveTypedef       Keyword " More precise is Typedef, but it doesn't feel right for Move
hi def link moveStructure     Keyword " More precise is Structure
hi def link moveUnion         moveStructure
hi def link moveExistential   moveKeyword
hi def link movePubScopeDelim Delimiter
hi def link movePubScopeCrate moveKeyword
hi def link moveSuper         moveKeyword
hi def link moveUnsafeKeyword Exception
hi def link moveReservedKeyword Error
hi def link moveRepeat        Conditional
hi def link moveConditional   Conditional
hi def link moveIdentifier    Identifier
hi def link moveCapsIdent     moveIdentifier
hi def link moveModPath       Include
hi def link moveModPathSep    Delimiter
hi def link moveFunction      Function
hi def link moveFuncName      Function
hi def link moveFuncCall      Function
hi def link moveShebang       Comment
hi def link moveCommentLine   Comment
hi def link moveCommentLineDoc SpecialComment
hi def link moveCommentLineDocLeader moveCommentLineDoc
hi def link moveCommentLineDocError Error
hi def link moveCommentBlock  moveCommentLine
hi def link moveCommentBlockDoc moveCommentLineDoc
hi def link moveCommentBlockDocStar moveCommentBlockDoc
hi def link moveCommentBlockDocError Error
hi def link moveCommentDocCodeFence moveCommentLineDoc
hi def link moveAssert        PreCondit
hi def link movePanic         PreCondit
hi def link moveMacro         Macro
hi def link moveType          Type
hi def link moveTodo          Todo
hi def link moveAttribute     PreProc
hi def link moveDerive        PreProc
hi def link moveDefault       StorageClass
hi def link moveStorage       StorageClass
hi def link moveObsoleteStorage Error
hi def link moveLifetime      Special
hi def link moveLabel         Label
hi def link moveExternCrate   moveKeyword
hi def link moveObsoleteExternMod Error
hi def link moveQuestionMark  Special
hi def link moveAsync         moveKeyword
hi def link moveAwait         moveKeyword
hi def link moveAsmDirSpec    moveKeyword
hi def link moveAsmSym        moveKeyword
hi def link moveAsmOptions    moveKeyword
hi def link moveAsmOptionsKey moveAttribute

" Other Suggestions:
" hi moveAttribute ctermfg=cyan
" hi moveDerive ctermfg=cyan
" hi moveAssert ctermfg=yellow
" hi movePanic ctermfg=red
" hi moveMacro ctermfg=magenta

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "move"

" vim: set et sw=4 sts=4 ts=8:
