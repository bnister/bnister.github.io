/* SPDX-License-Identifier: CC0-1.0 OR Unlicense
obfuscated OTCC and OTCCELF are public domain -- http://bellard.org/otcc/
otccelf.c self-compiling derivative work, feel free to study and share!

gcc -m32 -O2 otccelfpd.c -o otccelfpd_gcc -Wno-int-conversion -Wno-implicit-int -Wno-implicit-function-declaration

or self-hosted:

./otccelf1 otccelfpd.c otccelfpd
cmp otccelf1 otccelfpd */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifndef NULL
#define NULL 0
#endif

/* C89 grammar subset
identifier:
	identifier-nondigit
	identifier identifier-nondigit
	identifier digit
constant:
	integer-constant
	character-constant
integer-constant:
	decimal-constant
	octal-constant
	hexadecimal-constant
decimal-constant:
	nonzero-digit
	decimal-constant digit
octal-constant:
	0
	octal-constant octal-digit
hexadecimal-constant:
	hexadecimal-prefix hexadecimal-digit
	hexadecimal-constant hexadecimal-digit
hexadecimal-prefix:
	0x
	0X
character-constant:
	' c-char '
c-char:
	any-except-quote-backslash-new-line
	escape-sequence
escape-sequence:
	\'
	\"
	\?
	\\
	\n
string-literal:
	" s-char-sequence_opt "
s-char-sequence:
	c-char
	s-char-sequence c-char

primary-expression:
	identifier
	constant
	string-literal
	( expression )
unary-postfix-expression:
	primary-expression
	unary-operator unary-postfix-expression
	* ( type-name ) unary-postfix-expression
	* ( type-name ) unary-postfix-expression = expression
	& identifier
	identifier ++
	identifier --
	unary-postfix-expression ( argument-expression-list_opt )
argument-expression-list:
	expression
	argument-expression-list , expression
unary-operator:
	+
	-
	~
	!
assignment-expression:
	unary-postfix-expression
	identifier = expression
multiplicative-expression:
	assignment-expression
	multiplicative-expression * assignment-expression
	multiplicative-expression / assignment-expression
	multiplicative-expression % cast-expression
additive-expression:
	multiplicative-expression
	additive-expression + multiplicative-expression
	additive-expression - multiplicative-expression
shift-expression:
	additive-expression
	shift-expression << additive-expression
	shift-expression >> additive-expression
relational-expression:
	shift-expression
	relational-expression < shift-expression
	relational-expression > shift-expression
	relational-expression <= shift-expression
	relational-expression >= shift-expression
equality-expression:
	relational-expression
	equality-expression == relational-expression
	equality-expression != relational-expression
and-expression:
	equality-expression
	and-expression & equality-expression
exclusive-or-expression:
	and-expression
	exclusive-or-expression ^ and-expression
inclusive-or-expression:
	exclusive-or-expression
	inclusive-or-expression | exclusive-or-expression
logical-and-expression:
	inclusive-or-expression
	logical-and-expression && inclusive-or-expression
logical-or-expression:
	logical-and-expression
	logical-or-expression || logical-and-expression
expression:
	logical-or-expression

declaration:
	declaration-specifiers init-declarator-list_opt ;
declaration-specifiers:
	type-specifier
init-declarator-list:
	init-declarator
	init-declarator-list , init-declarator
init-declarator:
	declarator
type-specifier:
	int
specifier-qualifier-list:
	type-specifier
declarator:
	direct-declarator
direct-declarator:
	identifier
pointer:
	*
identifier-list:
	identifier
	identifier-list , identifier
type-name:
	specifier-qualifier-list abstract-declarator
	char-or-anything-not-int pointer
abstract-declarator:
	pointer
	direct-abstract-declarator
direct-abstract-declarator:
	( pointer ) ( )

statement:
	compound-statement
	expression-statement
	selection-statement
	iteration-statement
	jump-statement
compound-statement:
	{ declaration-list_opt statement-list_opt }
declaration-list:
	declaration
	declaration-list declaration
statement-list:
	statement
	statement-list statement
expression-statement:
	expression_opt ;
selection-statement:
	if ( expression ) statement
	if ( expression ) statement else statement
iteration-statement:
	while ( expression ) statement
	for ( expression_opt ; expression_opt ; expression_opt ) statement
jump-statement:
	break ;
	return expression_opt ;

translation-unit:
	external-declaration
	translation-unit external-declaration
external-declaration:
	function-definition
	declaration
function-definition:
	declarator ( identifier-list_opt ) statement

preprocessing-file:
	group_opt
group:
	group-part
	group group-part
group-part:
	control-line
	text-line
	# non-directive
control-line:
	# define identifier replacement-list new-line */

/* code, data and symbol sizes */
#define MAXLEN 99999

/* char flag marking the end of a macro definition */
#define ENDDEF 2

/* symbol table entry for a macro definition (string terminated by ENDDEF) */
#define SYMDEF 1
/* symbol table entry bound for stack-based operand */
#define SYMEBP 512

/* symbol table entry length (int32_t and pointer) */
#define SYMLEN 8
/* symbol table offset for reusing characters as tokens */
#define SYMOFF 0x100

#define TKLEN 48
#define TK " int if else while break return for define main "
/*          1   1  1    1     1     1      12  2      2 from	v
            00112233445566778899aabbccddeeff00112233445 space	v
            0808080808080808080808080808080808080808080 down	v */
#define TKEOF -1
#define TKIGNOR 1
#define TKCONST 2
#define TKINT	0x100
#define TKIF	0x120
#define TKELSE	0x138
#define TKWHILE	0x160
#define	TKBREAK	0x190
#define TKRET	0x1c0
#define TKFOR	0x1f8
#define TKDEF	0x218
#define TKLAST	0x218

#define SYMMAIN	0x250

#define OPTAB "++#m--%am*@R<^1c/@%[_[H3c%@%[_[H3c+@.B#d-@%:_^BKd<<Z/03e>>`/03e<=0f>=/f<@.f>@1f==&g!='g&&k||#l&@.BCh^@.BSi|@.B+j~@/%Yd!@&d*@b"
/* operators with x86 machine code (byte reversed, base 64) and precedence level
op	pr	tkv
++	11	1
--	11	-1
*	1	0xc1af0f	imul eax, ecx
/	1	0xf9f79991	xchg eax, ecx; cdq; idiv edx
%	1	0xf9f79991	-!-
+	2	0xc801	add eax, ecx
-	2	0xd8f7c829	sub eax, ecx; neg eax
<<	3	0xe0d391	xchg eax, ecx; shl eax, cl
>>	3	0xf8d391	xchg eax, ecx; sar eax, cl
<=	4	0xe	+c0 setle
>=	4	0xd	+c0 setge
<	4	0xc	+c0 setl
>	4	0xf	+c0 setg
==	5	0x4	+c0 sete
!=	5	0x5	+c0 setne
&&	9	0x0	+84 jz
||	10	0x1	+84 jnz
&	6	0xc821	and eax, ecx
^	7	0xc831	xor eax, ecx
|	8	0xc809	or eax, ecx
~	2	0xd0f7	not eax
!	2	0x4	+c0 sete
*	0	0 */

/* the order is important for binary identical self-compilation */
int tk, tkv, tkpr, lexc; /* token, reused as int* to sym; value, precedence */
int sym; /* struct{int;void*;}* symbol table indexed by position in str */
int pjret; /* struct n{struct n*}* list of jumps to epilogue */
int text, ptext; /* void* .text section, zero initialized in calloc */
int frame; /* callee's stack frame size in bytes */
int pdata; /* void* end of .data section */
int pfi; /* FILE* opened for reading input (argv[1]) */
int str, pstr; /* char* string of tokens and symbols much alike .strtab */
int pdef, defc; /* char* macro definition, saved lookahead character */
int pident; /* char* identifier */
int data; /* void* ELF headers + .data section, zero init in calloc */
int pentry; /* 0 when generating code, code entry point in data for ELF */
int vaoff; /* added to data pointers to get ELF virtual address space */

/* unary op priorities */
#define PRNEGATION	2
#define PRPOSTFIX	11

/* binary op priorities */
#define PRTERMSTOP	1
#define PRRELATION	4
#define PREQUALITY	5	
#define PRBOOLBOUND	8
#define PRBOOLSTART	11

#define PRIGNORED	0

strputc(c) {
	*(char*)pstr++ = c;
}

/* LL(2) lexical elements */

lexgetc() {
	if (pdef) {
		lexc = *(char*)pdef++;
		if (lexc == ENDDEF) {
			pdef = 0;
			lexc = defc;
		}
	}
	else lexc = fgetc(pfi); /* libc */
}

lexident() {
	return isalnum(lexc) | lexc == '_'; /* libc */
}

lexescape() {
	if (lexc == '\\') {
		lexgetc();
		if (lexc == 'n') lexc = '\n';
	}
}

lex() {
	while (isspace(lexc) | lexc == '#') { /* libc */
		if (lexc == '#') {
			lexgetc();
			lex(); /* get a token */
			if (tk == TKDEF) { /* it's either a #define */
				lex();
				strputc(' ');
				*(int*)tk = SYMDEF; /* SEGV if not an identifier FIXME */
				*(int*)(tk + 4) = pstr;
			}
			while (lexc != '\n') { /* or non-directive */
				strputc(lexc); /* excessive storage FIXME */
				lexgetc();
			}
			strputc(lexc); /* not part of macro in standard FIXME */
			strputc(ENDDEF);
		}
		lexgetc(); /* accept (and ignore) the whitespace character */
	}
	tkpr = PRIGNORED; /* when parsed by precedence */
	tk = lexc; /* the simpliest form of a token is the single character */
	if (lexident()) {
		strputc(' '); /* identifiers surrounded by spaces in str */
		pident = pstr;
		while (lexident()) {
			strputc(lexc);
			lexgetc();
		}
		if (isdigit(tk)) { /* libc */
			tkv = strtol(pident, NULL, 0); /* libc */
			tk = TKCONST; /* integer-constant */
		}
		else {
			*(char*)pstr = ' '; /* space-identifier-space to search */
			/* *(char*)(pstr + 1) = 0; // from calloc FIXME */
			tk = strstr(str, pident - 1) - str; /* libc token/symbol position */
			/* will find _itself_ in worst case, strstr params share a buffer */
			*(char*)pstr = 0; /* no idea. A fossil for dlsym in OTCC? */
			tk = tk * SYMLEN + SYMOFF;
			if (tk > TKLAST) { /* not a keyword (main predefined, but isn't!) */
				tk = sym + tk; /* token == pointer to symbol table!! */
				if (*(int*)tk == SYMDEF) { /* out of bounds SEGV FIXME */
					pdef = *(int*)(tk + 4);
					defc = lexc; /* macro tokens parsed before lookahead char */
					lexgetc();
					lex(); /* first token from the macro */
				}
			}
		}
	}
	else {
		lexgetc(); /* next lookahead char i.e. LL(2) part */
		if (tk == '\'') {
			tk = TKCONST; /* character-constant */
			lexescape();
			tkv = lexc;
			lexgetc(); /* assert(lexc == '\''); */
			lexgetc();
		}
		else if (tk == '/' & lexc == '*') { /* C89 comment */
			lexgetc();
			while (lexc) {
				while (lexc != '*') lexgetc();
				lexgetc();
				if (lexc == '/') lexc = 0;
			}
			lexgetc();
			lex(); /* first token after the comment */
		}
		else {
			int pt, c1, c2;
			pt = OPTAB;
			while ((c1 = *(char*)pt++)) {
				c2 = *(char*)pt++;
				tkv = 0; /* 34(35)..97 encode the value, 98..109 precedence */
				while ((tkpr = *(char*)pt++ - 98) < 0) tkv = tkv * 64 + tkpr + 64;
				if (c1 == tk & (c2 == lexc | c2 == '@')) {
					if (c2 == lexc) {
						lexgetc(); /* accept both lookaheads */
						tk = TKIGNOR; /* ++ -- parsed by precedence only */
					}
					break;
				}
			}
		}
	}
}

/* x86 machine code generator snippets */

/* Mod 00 REG 000 R/M 101 eax/al disp32 */
#define X86DISP32 5
/* Mod 10 REG 000 R/M 101 eax/al [ebp + disp32] */
#define X86EBP32 0x85

x86vlc(c) { /* little endian, variable length, last operand byte must be != 0 */
	while (c && c != -1) { /* -1 for the longest x86 sequences in OPTAB */
		*(char*)ptext++ = c;
		c = c >> 8; /* int is signed, shifts 1's if last op >= 0x80 */
	}
}

x86put4(p, c) {
	*(char*)p++ = c;
	*(char*)p++ = c >> 8;
	*(char*)p++ = c >> 16;
	*(char*)p++ = c >> 24;
}

x86get4(p) {
	int unused; /* for binary identical self-compilation */
	return (*(char*)p & 255) |
		(*(char*)(p + 1) & 255) << 8 |
		(*(char*)(p + 2) & 255) << 16 |
		(*(char*)(p + 3) & 255) << 24;
}

x86patch4(psrc, pdst) {
	int pnext;
	while (psrc) { /* linked list of undefined references to pdst */
		pnext = x86get4(psrc);
		if (*(char*)(psrc - 1) == X86DISP32) /* .data or .text reference */
			if (pdst >= data && pdst < pdata) x86put4(psrc, pdst + vaoff);
			else x86put4(psrc, pdst - text + pentry + vaoff);
		else x86put4(psrc, pdst - psrc - 4); /* [jxx/call] rel32 */
		psrc = pnext;
	}
}

x86link(p) { /* forward references to current sequence point */
	x86patch4(p, ptext);
}

x86ref(c, phead) {
	x86vlc(c); /* an instruction with imm32/disp32/rel32 operand */
	x86put4(ptext, phead);
	phead = ptext;
	ptext = ptext + 4;
	return phead; /* doubling as a new node in undefined references list */
}

x86imm(d) {
	x86ref(0xb8, d); /* mov eax, imm32 */
}

x86jmp(p) {
	return x86ref(0xe9, p); /* jmp rel32 */
}

/* jump if false for control statements */
#define X86JZ 0

x86test(nz, p) { /* boolean logic evaluation and control statements */
	x86vlc(0x0fc085); /* test eax, eax */
	return x86ref(0x84 + nz, p); /* jz/jnz rel32 */
}

x86cmp(cc) {
	x86vlc(0xc139); /* cmp ecx, eax */
	x86imm(0);
	x86vlc(0x0f);
	x86vlc(cc + 0x90);
	x86vlc(0xc0); /* setcc al */
}

x86mem(c, psym) { /* variable manipulation */
	int entry;
	x86vlc(c + 0x83); /* add imm8 / mov / lea */
	entry = *(int*)psym;
	if (entry && entry < SYMEBP) x86ref(X86EBP32, entry);
	else { /* always an undefined forward reference for globals */
		psym = psym + 4; /* insert a new node to the linked list */
		*(int*)psym = x86ref(X86DISP32, *(int*)psym);
	}
}

/* LL(2) recursive descent parser generating x86 machine code on the fly */

/* component of an expression */
#define TERMUNARYPOSTFIX 0
#define TERMASSIGNMENT 1

/* declaration scope */
#define DECLLIST 1
#define DECLUNIT 0

#define assignment off
synterm(off) { /* c.f. terminating case of recursive AST traversal */
	int funcptr, tk1, val, precedence;
	funcptr = 1;
	if (tk == '"') { /* string-literal */
		x86imm(pdata + vaoff); /* eax = pointer to the string */
		while (lexc != '"') { /* store the string to .data */
			lexescape();
			*(char*)pdata++ = lexc;
			lexgetc();
		}
		*(char*)pdata = 0;
		pdata = pdata + 4 & -4; /* realign to avoid performance penalties */
		lexgetc();
		lex();
	}
	else { /* LL(2) part */
		precedence = tkpr;
		val = tkv;
		tk1 = tk;
		lex();
		if (tk1 == TKCONST) x86imm(val);
		else if (precedence == PRNEGATION) {
			synterm(TERMUNARYPOSTFIX);
			x86ref(0xb9, 0); /* mov ecx, imm32 */
			if (tk1 == '!') x86cmp(val); /* 1 if zero, 0 if not thus negation */
			else x86vlc(val);
		}
		else if (tk1 == '(') { /* parenthesized expression */
			synexpr();
			lex(); /* ')' */
		}
		else if (tk1 == '*') { /* indirection */
			lex(); /* '(' */
			tk1 = tk; /* type-specifier */
			lex();
			lex(); /* '*' '(' */
			if (tk == '*') { /* function pointer *(int (*)()) */
				lex(); /* '*' */
				lex(); /* ')' */
				lex(); /* '(' */
				lex(); /* ')' */
				tk1 = 0; /* eax already points to a function */
			}
			lex(); /* ')' */
			synterm(TERMUNARYPOSTFIX);
			if (tk == '=') { /* lvalue indirection */
				lex();
				x86vlc(0x50); /* push eax */
				synexpr();
				x86vlc(0x59); /* pop ecx */
				x86vlc(0x0188 + (tk1 == TKINT)); /* mov [ecx], al/eax */
			} /* *(main*), *(undef_ident*) or *(+*) same as *(char*) FIXME */
			else if (tk1) { /* eax points to an object */
				if (tk1 == TKINT) x86vlc(0x8b); /* mov r32, r/m32 */
				else x86vlc(0xbe0f); /* movsx r32, r/m8 */
				ptext++; /* skip over calloc'd ModR/M 0 => eax, [eax] */
			}
		}
		else if (tk1 == '&') { /* address */
			x86mem(10, tk); /* 8dx5 lea eax, m */
			lex();
		}
		else { /* assert(tk1 > TKLAST); // identifier in tk1 */
			funcptr = 0;
			/* assert(TERMUNARYPOSTFIX == 0 && TERMASSIGNMENT == 1); */
			if (tk == '=' & assignment) { /* a = b + c = d FIXME */
				lex();
				synexpr();
				x86mem(6, tk1); /* 89x5 mov m, eax */
			}
			else if (tk != '(') { /* load a value if no function call ahead */
				x86mem(8, tk1); /* 8bx5 mov eax, m */
				if (tkpr == PRPOSTFIX) {
					x86mem(0, tk1); /* 83x5 add dword ptr m, imm8 */
					x86vlc(tkv); /* 1 for ++ or -1 for -- */
					lex(); /* TKIGNOR */
				}
			} /* funcptr++() is legal if only looking weird! FIXME */
		}
	}
	/* ""(); 0(); ~f()(); *(int (*)())func()(); &func(); FIXME */
	if (tk == '(') { /* function call */
		if (funcptr) x86vlc(0x50); /* push eax */
		val = x86ref(0xec81, 0); /* sub esp, imm32 */
		lex();
		off = 0;
		while (tk != ')') {
			synexpr();
			x86ref(0x248489, off); /* mov dword ptr [esp + disp32], eax */
			if (tk == ',') lex();
			off = off + 4; /* i386 SysV ABI (cdecl) order, upward in hw stack */
		}
		x86put4(val, off); /* patch sub esp with caller's frame size */
		lex();
		if (funcptr) { /* these are super rare, disp32 of negligible impact */
			x86ref(0x2494ff, off); /* call dword ptr [esp + disp32] */
			off = off + 4;
		}
		else { /* always an undefined forward reference */
			tk1 = tk1 + 4; /* insert a new node to the linked list */
			*(int*)tk1 = x86ref(0xe8, *(int*)tk1); /* call rel32 */
		}
		if (off) x86ref(0xc481, off); /* add esp, imm32 */
	}
}

synbinary(precedence) { /* convert infix notation to x86 stack machine */
	int val, tk1, pjmp;
	if (precedence-- == PRTERMSTOP) synterm(TERMASSIGNMENT);
	else {
		synbinary(precedence); /* left side */
		pjmp = NULL;
		while (precedence == tkpr) {
			tk1 = tk;
			val = tkv;
			lex();
			if (precedence > PRBOOLBOUND) {
				pjmp = x86test(val, pjmp); /* short circuit evaluation */
				synbinary(precedence); /* right side */
			}
			else {
				x86vlc(0x50); /* push eax */
				synbinary(precedence); /* right side */
				x86vlc(0x59); /* pop ecx */
				if (precedence == PRRELATION | precedence == PREQUALITY) {
					x86cmp(val); /* cmp + setcc monster */
				}
				else {
					x86vlc(val); /* substraction (!) and division reversed */
					if (tk1 == '%') x86vlc(0x92); /* xchg eax, edx modulo */
				}
			}
		}
		if (pjmp && precedence > PRBOOLBOUND) {
			pjmp = x86test(val, pjmp);
			x86imm(val ^ 1); /* false for || full evaluation, true for && */
			x86jmp(5); /* jmp $ + 5 */
			x86link(pjmp); /* short circuit destination */
			x86imm(val); /* true for || short circuit, false for && */
		}
	}
}

synexpr() {
	synbinary(PRBOOLSTART);
}

syntest() {
	synexpr();
	return x86test(X86JZ, NULL);
}

#define statement_token pjbody
synstatement(pjbrk) { /* to end of innermost loop propagated through */
	int pjtest, ploop, pjbody;
	if (tk == TKIF) { /* selection-statement */
		lex();
		lex(); /* '(' */
		pjtest = syntest(); /* to next statement or else statement if false */
		lex(); /* ')' */
		synstatement(pjbrk);
		if (tk == TKELSE) {
			lex();
			ploop = x86jmp(NULL); /* to next statement */
			x86link(pjtest);
			synstatement(pjbrk);
			x86link(ploop);
		}
		else {
			x86link(pjtest);
		}
	}
	else if (tk == TKWHILE | tk == TKFOR) { /* iteration-statement */
		statement_token = tk;
		lex();
		lex(); /* '(' */
		if (statement_token == TKWHILE) {
			ploop = ptext; /* absolute loop address */
			pjtest = syntest(); /* break out of loop if false */
		}
		else {
			if (tk != ';') synexpr(); /* initialization */
			lex(); /* ';' */
			ploop = ptext;
			pjtest = NULL;
			if (tk != ';') pjtest = syntest(); /* controlling expression */
			lex(); /* ';' */
			if (tk != ')') {
				pjbody = x86jmp(NULL); /* to loop body over incrementing */
				synexpr(); /* operation performed after each iteration */
				x86jmp(ploop - ptext - 5); /* rel32 back to controlling */
				x86link(pjbody);
				ploop = pjbody + 4; /* now loop to incrementing expression */
			}
		}
		lex(); /* ')' */
		synstatement(&pjtest); /* loop body */
		x86jmp(ploop - ptext - 5); /* rel32 back to controlling or increment */
		x86link(pjtest);
	}
	else if (tk == '{') { /* compound-statement */
		lex();
		syndecl(DECLLIST);
		while (tk != '}') synstatement(pjbrk);
		lex();
	}
	else { /* jump-statement */
		if (tk == TKRET) {
			lex();
			if (tk != ';') synexpr();
			pjret = x86jmp(pjret); /* to function epilogue */
		}
		else if (tk == TKBREAK) {
			lex();
			*(int*)pjbrk = x86jmp(*(int*)pjbrk); /* modifying a local! */
		}
		else { /* expression-statement */
			if (tk != ';') synexpr();
		}
		lex(); /* ';' */
	}
}

syndecl(list) {
	int off;
	/* assert(DECLLIST == 1 && DECLUNIT == 0); */
	/* declaration-list | translation-unit */
	while (tk == TKINT | tk != TKEOF & !list) {
		if (tk == TKINT) { /* the one and only type-specifier allowed */
			lex();
			while (tk != ';') {
				if (list) { /* locals from any compound-statement */
					frame = frame + 4;
					*(int*)tk = -frame; /* < SYMEBP since it's negative */
				}
				else {
					*(int*)tk = pdata; /* global variable in .data */
					pdata = pdata + 4; /* initialized implicitly by calloc */
				}
				lex();
				if (tk == ',') lex();
			}
			lex(); /* ';' */
		}
		else { /* function-definition */
			*(int*)tk = ptext; /* store the symbol table entry */
			lex();
			lex(); /* '(' */
			off = 8; /* over saved ebp and return address */
			while (tk != ')') { /* assert(off < SYMEBP); */
				*(int*)tk = off; /* named parameter in caller's stack frame */
				off = off + 4; /* i386 SysV ABI (cdecl) order, up in hw stack */
				lex();
				if (tk == ',') lex();
			}
			lex();
			pjret = frame = 0; /* pjret = NULL actually */
			x86vlc(0xe58955); /* push ebp; mov ebp, esp prologue */
			off = x86ref(0xec81, 0); /* sub esp, imm32 for locals */
			synstatement(NULL); /* allows illegal main() return; FIXME */
			x86link(pjret); /* return statements' destination */
			x86vlc(0xc3c9); /* leave; ret epilogue */
			x86put4(off, frame); /* patch sub esp with size from synstatement */
		}
	}
}

#define VAELF 0x8048000

/* Elf32_Ehdr e_type ET_EXEC, machine EM_386 */
#define MACH	0x30002
#define PHOFF	0x30
#define PHNUM	3
#define HDRSIZE	0x200034
/* e_ehsize 0x34, phentsize 0x20; Elf32_Phdr[3] */
#define LDOFF	0x90
#define LDFNAME	"/lib/ld-linux.so.2"
#define LDSIZE	19
#define LDALIGN	20
/* 0x90 + 20, Elf32_Dyn[11] */
#define DYNOFF	0xa4
#define DYNSIZE	88
#define DATAOFF	0xfc

#define ENTRYSZ	17

#define DT_NEEDED 1
#define LIBCST	1
#define LIBCFN	"libc.so.6"
#define LIBCSZ	10
/* OTCCELF itself doesn't use libdl, can compile OTCC though */
#define LIBDLST	11
#define LIBDLFN	"libdl.so.2"
#define LIBDLSZ	11
/* LIBDLST + LIBDLSZ */
#define STRTSYM	22

#define STRTAB 0
#define SYMTAB 1
#define REL 2

/* ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE) | STV_DEFAULT << 8 | SHN_UNDEF << 16 */
#define ST_GLOBAL_UNDEF 16

/* disp32 */
#define R_386_32	1
/* rel32 */
#define R_386_PC32	2

elf4(c) { /* a short name for many calls */
	x86put4(pdata, c);
	pdata = pdata + 4;
}

elfphdr(off, sz) { /* Program Header file offset, virtual address and sizes */
	elf4(off); /* p_offset */
	off = off + VAELF;
	elf4(off); /* p_vaddr */
	elf4(off); /* p_paddr */
	elf4(sz); /* p_filesz */
	elf4(sz); /* p_memsz */
}

elflink(section) { /* */
	int pc, ps, psym, index, entry, rel32;
	index = 0; /* into .strtab or .symtab */
	pc = str;
	while (1) {
		pc++;
		ps = pc;
		while (*(char*)pc != ' ' && pc < pstr) pc++; /* next symbol */
		if (pc == pstr) break;
		tk = sym + (ps - str) * SYMLEN + SYMOFF - SYMLEN; /* ps - str - 1 */
		entry = *(int*)tk;
		psym = *(int*)(tk + 4);
		if (psym && entry != SYMDEF) {
			if (!entry) { /* undefined reference */
				if (!section) { /* == STRTAB String Table .strtab */
					memcpy(pdata, ps, pc - ps); /* libc */
					pdata = pdata + pc - ps + 1;
				}
				else if (section == SYMTAB) { /* Symbol Table .symtab */
					elf4(index + STRTSYM); /* st_name */
					elf4(0); /* st_value */
					elf4(0); /* st_size no size or an unknown size */
					elf4(ST_GLOBAL_UNDEF); /* st_info */
					index = index + pc - ps + 1;
				}
				else { /* Relocation .rel */
					index++; /* skip STN_UNDEF with preincrement */
					while (psym) {
						ps = x86get4(psym);
						rel32 = *(char*)(psym - 1) != X86DISP32;
						x86put4(psym, -rel32 * 4);
						elf4(psym - text + pentry + vaoff); /* r_offset */
						/* assert(R_386_PC32 == 1 + R_386_32); */
						elf4(index * 256 + rel32 + R_386_32); /* r_info */
						psym = ps;
					}
				}
			} /* assert(entry >= SYMEBP); // global variable or function */
			else if (!section) { /* == 0 link only on the first call */
				x86patch4(psym, entry);
			}
		}
	}
}

elfwrite(filename) {
	int pend, pstrtab, strsz, psymtab, phash, prel, nchain, tmp, textsz;
	pentry = pdata;
	textsz = ptext - text;
	ptext = text; /* rewind to the space reserved for the entry point */
	x86vlc(0x505458); /* pop eax; push esp; push eax */
	tmp = *(int*)(sym + SYMMAIN);
	x86ref(0xe8, tmp - ptext - 5); /* call main */
	x86vlc(0xc389); /* mov ebx, eax */
	x86imm(1);
	x86vlc(0x80cd); /* int 80h */
	/* assert(ptext - text == ENTRYSZ); */	
	pdata = pdata + textsz; /* fast forward over space reserved for code */
	pstrtab = pdata;
	pdata++; /* skip over calloc'd null character for index zero */
	pdata = strcpy(pdata, LIBCFN) + LIBCSZ; /* strcpy from libc */
	pdata = strcpy(pdata, LIBDLFN) + LIBDLSZ; /* strcpy from libc */
	elflink(STRTAB);
	strsz = pdata - pstrtab;
	pdata = pdata + 3 & -4; /* realign to 32-bit boundary */
	psymtab = pdata;
	elf4(0); /* st_name No name for STN_UNDEF */
	elf4(0); /* st_value Zero value */
	elf4(0); /* st_size No size */
	elf4(0); /* st_info ST_NONE STB_LOCAL, other STV_DEFAULT, shndx SHN_UNDEF */
	elflink(SYMTAB);
	phash = pdata;
	nchain = (pdata - psymtab) / 16; /* sizeof(Elf32_Sym) */
	elf4(1); /* nbucket */
	elf4(nchain);
	elf4(1); /* bucket[0] */
	elf4(0); /* chain[0] STN_UNDEF */
	tmp = 2;
	while (tmp < nchain) elf4(tmp++); /* chain[tmp - 1] tmp */
	elf4(0); /* chain[nchain - 1] STN_UNDEF */
	prel = pdata;
	elflink(REL);
	memcpy(pentry, text, textsz); /* libc */
	pend = pdata;
	pdata = data; /* rewind to the space reserved for ELF headers */
	/* Elf32_Ehdr */
	elf4(0x464c457f); /* ELF magic number */
	elf4(0x10101); /* EI_CLASS 32, DATA 2LSB, VERSION EV_CURRENT, OSABI NONE */
	elf4(0); /* EI_ABIVERSION unspecified, PAD */
	elf4(0);
	elf4(MACH); /* e_type, machine */
	elf4(1); /* e_version EV_CURRENT */
	elf4(pentry + vaoff); /* e_entry */
	elf4(PHOFF);
	elf4(0); /* e_shoff */
	elf4(0); /* e_flags */
	elf4(HDRSIZE); /* e_ehsize, phentsize */
	elf4(PHNUM); /* e_phnum, e_shentsize 0 */
	/* char *pph = pdata; assert(pdata - data == PHOFF); */
	elf4(3); /* PT_INTERP overlaps invalid e_shnum 3, shstrndx 0 FIXME */
	elfphdr(LDOFF, LDSIZE);
	elf4(4); /* PF_R */
	elf4(1);
	/* Elf32_Phdr */
	elf4(1); /* PT_LOAD */
	elfphdr(0, pend - data); /* p_offset vaddr paddr filesz memsz */
	elf4(7); /* p_flags PF_R | PF_W | PF_X */
	elf4(4096); /* p_align x86 page */
	/* dynamic "segment" header */
	elf4(2); /* PT_DYNAMIC */
	elfphdr(DYNOFF, DYNSIZE);
	elf4(6); /* PF_R | PF_W */
	elf4(4);
	/* assert(pdata - pph == PHNUM * sizeof(Elf32_Phdr)) */
	pdata = strcpy(pdata, LDFNAME) + LDALIGN; /* strcpy from libc */
	/* char *pdyn = pdata; */
	elf4(DT_NEEDED);
	elf4(LIBCST);
	elf4(DT_NEEDED);
	elf4(LIBDLST);
	elf4(4);	/* DT_HASH */ 
	elf4(phash + vaoff);
	elf4(6);	/* DT_SYMTAB */
	elf4(psymtab + vaoff);
	elf4(5);	/* DT_STRTAB */
	elf4(pstrtab + vaoff);
	elf4(10);	/* DT_STRSZ */
	elf4(strsz);
	elf4(11);	/* DT_SYMENT */
	elf4(16);	/* sizeof(Elf32_Sym) */
	elf4(17);	/* DT_REL */
	elf4(prel + vaoff);
	elf4(18);	/* DT_RELSZ */
	elf4(pend - prel);
	elf4(19);	/* DT_RELENT */
	elf4(8);	/* sizeof(Elf32_Rel) */
	elf4(0);	/* DT_NULL */
	elf4(0);
	/* assert(pdata - pdyn == DYNSIZE); */
	tmp = fopen(filename, "w"); /* libc */
	fwrite(data, 1, pend - data, tmp); /* libc */
	fclose(tmp); /* libc */
}

main(argc, argv) {
	if (argc < 3) {
		printf("usage: otccelf file.c outfile\n"); /* libc */
		return 0;
	}
	/* should have been / SYMLEN (no IOCCC char limit for OTCCELF) FIXME */
	pstr = strcpy(str = calloc(1, MAXLEN), TK) + TKLEN; /* libc (twice) */
	pdata = data = calloc(1, MAXLEN); /* libc */
	ptext = text = calloc(1, MAXLEN); /* libc */
	sym = calloc(1, MAXLEN); /* libc */
	argv = argv + 4; /* N.B. (argv + 4) in place generates less code */
	pfi = fopen(*(int*)argv, "r"); /* libc */
	vaoff = VAELF - data;
	pdata = pdata + DATAOFF;
	ptext = ptext + ENTRYSZ;
	lexgetc();
	lex();
	syndecl(DECLUNIT);
	argv = argv + 4;
	elfwrite(*(int*)argv); /* (argv + 8) */
	return 0;
}