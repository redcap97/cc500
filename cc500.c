/*
 * Copyright (C) 2006 Edmund GRIMLEY EVANS <edmundo@rano.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 * A self-compiling compiler for a small subset of C.
 */

/* Our library functions. */
void exit(int);
int getchar(void);
void *malloc(int);
int putchar(int);

/* The first thing defined must be main(). */
int main1();
int main()
{
  return main1();
}

char *my_realloc(char *old, int oldlen, int newlen)
{
  char *new = malloc(newlen);
  int i = 0;
  while (i <= oldlen - 1) {
    new[i] = old[i];
    i = i + 1;
  }
  return new;
}

int nextc;
char *token;
int token_size;

void error()
{
  exit(1);
}

int i;

void takechar()
{
  if (token_size <= i + 1) {
    int x = (i + 10) << 1;
    token = my_realloc(token, token_size, x);
    token_size = x;
  }
  token[i] = nextc;
  i = i + 1;
  nextc = getchar();
}

void get_token()
{
  int w = 1;
  while (w) {
    w = 0;
    while ((nextc == ' ') | (nextc == 9) | (nextc == 10))
      nextc = getchar();
    i = 0;
    while ((('a' <= nextc) & (nextc <= 'z')) |
	   (('0' <= nextc) & (nextc <= '9')) | (nextc == '_'))
      takechar();
    if (i == 0)
      while ((nextc == '<') | (nextc == '=') | (nextc == '>') |
	     (nextc == '|') | (nextc == '&') | (nextc == '!'))
	takechar();
    if (i == 0) {
      if (nextc == 39) {
	takechar();
	while (nextc != 39)
	  takechar();
	takechar();
      }
      else if (nextc == '"') {
	takechar();
	while (nextc != '"')
	  takechar();
	takechar();
      }
      else if (nextc == '/') {
	takechar();
	if (nextc == '*') {
	  nextc = getchar();
	  while (nextc != '/') {
	    while (nextc != '*')
	      nextc = getchar();
	    nextc = getchar();
	  }
	  nextc = getchar();
	  w = 1;
	}
      }
      else if (nextc != 0-1)
	takechar();
    }
    token[i] = 0;
  }
}

int peek(char *s)
{
  int i = 0;
  while ((s[i] == token[i]) & (s[i] != 0))
    i = i + 1;
  return s[i] == token[i];
}

int accept(char *s)
{
  if (peek(s)) {
    get_token();
    return 1;
  }
  else
    return 0;
}

void expect(char *s)
{
  if (accept(s) == 0)
    error();
}

char *code;
int code_size;
int codepos;
int code_offset;

void save_int(char *p, int n)
{
  p[0] = n;
  p[1] = n >> 8;
  p[2] = n >> 16;
  p[3] = n >> 24;
}

void save_imm24(char *p, int n) {
  p[0] = n;
  p[1] = n >> 8;
  p[2] = n >> 16;
}

int encode_pc_relative(int ra) {
  return (ra - 8) >> 2;
}

int load_int(char *p)
{
  return ((p[0] & 255) + ((p[1] & 255) << 8) +
	  ((p[2] & 255) << 16) + ((p[3] & 255) << 24));
}

void emit(int n, char *s)
{
  i = 0;
  if (code_size <= codepos + n) {
    int x = (codepos + n) << 1;
    code = my_realloc(code, code_size, x);
    code_size = x;
  }
  while (i <= n - 1) {
    code[codepos] = s[i];
    codepos = codepos + 1;
    i = i + 1;
  }

  /* data must be 4-byte aligned */
  while (codepos & 3) {
    code[codepos] = 0;
    codepos = codepos + 1;
  }
}

void be_push()
{
  emit(4, "\x04\x00\x2d\xe5"); /* push {r0} */
}

void be_pop(int n)
{
  /* ldr r4, [pc]; b . + 8; the word; add sp, sp, r4 */
  emit(16, "\x00\x40\x9f\xe5\x00\x00\x00\xea....\x04\xd0\x8d\xe0");
  save_int(code + codepos - 8, n << 2);
}

char *table;
int table_size;
int table_pos;
int stack_pos;

int sym_lookup(char *s)
{
  int t = 0;
  int current_symbol = 0;
  while (t <= table_pos - 1) {
    i = 0;
    while ((s[i] == table[t]) & (s[i] != 0)) {
      i = i + 1;
      t = t + 1;
    }
    if (s[i] == table[t])
      current_symbol = t;
    while (table[t] != 0)
      t = t + 1;
    t = t + 6;
  }
  return current_symbol;
}

void sym_declare(char *s, int type, int value)
{
  int t = table_pos;
  i = 0;
  while (s[i] != 0) {
    if (table_size <= t + 10) {
      int x = (t + 10) << 1;
      table = my_realloc(table, table_size, x);
      table_size = x;
    }
    table[t] = s[i];
    i = i + 1;
    t = t + 1;
  }
  table[t] = 0;
  table[t + 1] = type;
  save_int(table + t + 2, value);
  table_pos = t + 6;
}

int sym_declare_global(char *s)
{
  int current_symbol = sym_lookup(s);
  if (current_symbol == 0) {
    sym_declare(s, 'U', code_offset);
    current_symbol = table_pos - 6;
  }
  return current_symbol;
}

void sym_define_global(int current_symbol)
{
  int i;
  int j;
  int t = current_symbol;
  int v = codepos + code_offset;
  if (table[t + 1] != 'U')
    error(); /* symbol redefined */
  i = load_int(table + t + 2) - code_offset;
  while (i) {
    j = load_int(code + i) - code_offset;
    save_int(code + i, v);
    i = j;
  }
  table[t + 1] = 'D';
  save_int(table + t + 2, v);
}

int number_of_args;

void sym_get_value(char *s)
{
  int t;
  if ((t = sym_lookup(s)) == 0)
    error();
  /* ldr r4, [pc]; b . + 8; the word; mov r0, r4 */
  emit(16, "\x00\x40\x9f\xe5\x00\x00\x00\xea....\x04\x00\xa0\xe1");
  save_int(code + codepos - 8, load_int(table + t + 2));
  if (table[t + 1] == 'D') { /* defined global */
  }
  else if (table[t + 1] == 'U') /* undefined global */
    save_int(table + t + 2, codepos + code_offset - 8);
  else if (table[t + 1] == 'L') { /* local variable */
    int k = (stack_pos - table[t + 2] - 1) << 2;
    /* ldr r4, [pc]; b . + 8; the word; add r0, sp, r4 */
    emit(16, "\x00\x40\x9f\xe5\x00\x00\x00\xea....\x04\x00\x8d\xe0");
    save_int(code + codepos - 8, k);
  }
  else if (table[t + 1] == 'A') { /* argument */
    int k = (stack_pos + number_of_args - table[t + 2]) << 2;
    /* ldr r4, [pc]; b . + 8; the word; add r0, sp, r4 */
    emit(16, "\x00\x40\x9f\xe5\x00\x00\x00\xea....\x04\x00\x8d\xe0");
    save_int(code + codepos - 8, k);
  }
  else
    error();
}

void be_start()
{
  emit(16, "\x7f\x45\x4c\x46\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00");
  emit(16, "\x02\x00\x28\x00\x01\x00\x00\x00\x54\x80\x00\x00\x34\x00\x00\x00");
  emit(16, "\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x20\x00\x01\x00\x00\x00");
  emit(16, "\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00");
  emit(16, "\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00");
  emit(16, "\x00\x80\x00\x00\x00\x00\x00\xeb\x01\x70\xa0\xe3\x00\x00\x00\xef");

  sym_define_global(sym_declare_global("exit"));
  /* pop {r0}; mov r7, #1; svc 0x00000000; bx lr */
  emit(16, "\x04\x00\x9d\xe4\x01\x70\xa0\xe3\x00\x00\x00\xef\x1e\xff\x2f\xe1");

  sym_define_global(sym_declare_global("getchar"));
  /* mov r7, #3; mov r0, #0; push {r0}; mov r1, sp */
  emit(16, "\x03\x70\xa0\xe3\x00\x00\xa0\xe3\x04\x00\x2d\xe5\x0d\x10\xa0\xe1");
  /* mov r2, #1; svc 0x00000000; tst r0, r0; pop {r0} */
  emit(16, "\x01\x20\xa0\xe3\x00\x00\x00\xef\x00\x00\x10\xe1\x04\x00\x9d\xe4");
  /* bne . + 8; mvn r0, #0; bx lr */
  emit(12, "\x00\x00\x00\x1a\x00\x00\xe0\xe3\x1e\xff\x2f\xe1");

  sym_define_global(sym_declare_global("malloc"));
  /* mov r7, #45; mov r0, #0; svc 0x00000000; mov r1, r0 */
  emit(16, "\x2d\x70\xa0\xe3\x00\x00\xa0\xe3\x00\x00\x00\xef\x00\x10\xa0\xe1");
  /* ldr r0, [sp]; add r0, r0, r1; push {r1}; push {r0} */
  emit(16, "\x00\x00\x9d\xe5\x01\x00\x80\xe0\x04\x10\x2d\xe5\x04\x00\x2d\xe5");
  /* svc 0x00000000; pop {r1}; cmp r0, r1; pop {r0} */
  emit(16, "\x00\x00\x00\xef\x04\x10\x9d\xe4\x01\x00\x50\xe1\x04\x00\x9d\xe4");
  /* beq . + 8; mvn r0, #0; bx lr */
  emit(12, "\x00\x00\x00\x0a\x00\x00\xe0\xe3\x1e\xff\x2f\xe1");

  sym_define_global(sym_declare_global("putchar"));
  /* mov r7, #4; mov r0, #1; mov r1, sp; mov r2, #1 */
  emit(16, "\x04\x70\xa0\xe3\x01\x00\xa0\xe3\x0d\x10\xa0\xe1\x01\x20\xa0\xe3");
  /* svc 0x00000000; bx lr */
  emit(8,  "\x00\x00\x00\xef\x1e\xff\x2f\xe1");

  save_imm24(code + 84, encode_pc_relative(codepos - 84)); /* entry set to first thing in file */
}

void be_finish()
{
  save_int(code + 68, codepos);
  save_int(code + 72, codepos);
  i = 0;
  while (i <= codepos - 1) {
    putchar(code[i]);
    i = i + 1;
  }
}

void promote(int type)
{
  /* 1 = char lval, 2 = int lval, 3 = other */
  if (type == 1)
    emit(4, "\x00\x00\xd0\xe5"); /* ldrb r0, [r0] */
  else if (type == 2)
    emit(4, "\x00\x00\x90\xe5"); /* ldr r0, [r0] */
}

int expression();

/*
 * primary-expr:
 *     identifier
 *     constant
 *     ( expression )
 */
int primary_expr()
{
  int type;
  if (('0' <= token[0]) & (token[0] <= '9')) {
    int n = 0;
    i = 0;
    while (token[i]) {
      n = (n << 1) + (n << 3) + token[i] - '0';
      i = i + 1;
    }
    /* ldr r4, [pc]; b . + 8; the word; mov r0, r4 */
    emit(16, "\x00\x40\x9f\xe5\x00\x00\x00\xea....\x04\x00\xa0\xe1");
    save_int(code + codepos - 8, n);
    type = 3;
  }
  else if (('a' <= token[0]) & (token[0] <= 'z')) {
    sym_get_value(token);
    type = 2;
  }
  else if (accept("(")) {
    type = expression();
    if (peek(")") == 0)
      error();
  }
  else if ((token[0] == 39) & (token[1] != 0) &
	   (token[2] == 39) & (token[3] == 0)) {
    /* ldr r4, [pc]; b . + 8; the word; mov r0, r4 */
    emit(16, "\x00\x40\x9f\xe5\x00\x00\x00\xea....\x04\x00\xa0\xe1");
    save_int(code + codepos - 8, token[1]);
    type = 3;
  }
  else if (token[0] == '"') {
    int i = 0;
    int j = 1;
    int k;
    int p;
    while (token[j] != '"') {
      if ((token[j] == 92) & (token[j + 1] == 'x')) {
	if (token[j + 2] <= '9')
	  k = token[j + 2] - '0';
	else
	  k = token[j + 2] - 'a' + 10;
	k = k << 4;
	if (token[j + 3] <= '9')
	  k = k + token[j + 3] - '0';
	else
	  k = k + token[j + 3] - 'a' + 10;
	token[i] = k;
	j = j + 4;
      }
      else {
	token[i] = token[j];
	j = j + 1;
      }
      i = i + 1;
    }
    token[i] = 0;
    /* mov r0, pc; b . + sizeof(the string); the string */
    emit(8, "\x0f\x00\xa0\xe1\x00\x00\x00\xea");
    p = codepos;
    emit(i + 1, token);
    save_imm24(code + p - 4, encode_pc_relative(codepos - p + 4));
    type = 3;
  }
  else
    error();
  get_token();
  return type;
}

void binary1(int type)
{
  promote(type);
  be_push();
  stack_pos = stack_pos + 1;
}

int binary2(int type, int n, char *s)
{
  promote(type);
  emit(n, s);
  stack_pos = stack_pos - 1;
  return 3;
}

/*
 * postfix-expr:
 *         primary-expr
 *         postfix-expr [ expression ]
 *         postfix-expr ( expression-list-opt )
 */
int postfix_expr()
{
  int type = primary_expr();
  if (accept("[")) {
    binary1(type); /* pop {r1}; add r0, r0, r1 */
    binary2(expression(), 8, "\x04\x10\x9d\xe4\x01\x00\x80\xe0");
    expect("]");
    type = 1;
  }
  else if (accept("(")) {
    int s;

    emit(4, "\x04\xe0\x2d\xe5"); /* push {lr} */
    stack_pos = stack_pos + 1;

    s = stack_pos;
    be_push();
    stack_pos = stack_pos + 1;
    if (accept(")") == 0) {
      promote(expression());
      be_push();
      stack_pos = stack_pos + 1;
      while (accept(",")) {
	promote(expression());
	be_push();
	stack_pos = stack_pos + 1;
      }
      expect(")");
    }
    /* ldr r4, [pc]; b . + 8; the word; ldr r0, [sp, r4] */
    emit(16, "\x00\x40\x9f\xe5\x00\x00\x00\xea....\x04\x00\x9d\xe7");
    /* mov lr, pc; mov pc, r0 */
    emit(8,  "\x0f\xe0\xa0\xe1\x00\xf0\xa0\xe1");
    save_int(code + codepos - 16, (stack_pos - s - 1) << 2);

    be_pop(stack_pos - s);
    stack_pos = s;

    emit(4, "\x04\xe0\x9d\xe4"); /* pop {lr} */
    stack_pos = stack_pos - 1;

    type = 3;
  }
  return type;
}

/*
 * additive-expr:
 *         postfix-expr
 *         additive-expr + postfix-expr
 *         additive-expr - postfix-expr
 */
int additive_expr()
{
  int type = postfix_expr();
  while (1) {
    if (accept("+")) {
      binary1(type); /* pop {r1}; add r0, r1, r0 */
      type = binary2(postfix_expr(), 8, "\x04\x10\x9d\xe4\x00\x00\x81\xe0");
    }
    else if (accept("-")) {
      binary1(type); /* pop {r1}; sub r0, r1, r0 */
      type = binary2(postfix_expr(), 8, "\x04\x10\x9d\xe4\x00\x00\x41\xe0");
    }
    else
      return type;
  }
}

/*
 * shift-expr:
 *         additive-expr
 *         shift-expr << additive-expr
 *         shift-expr >> additive-expr
 */
int shift_expr()
{
  int type = additive_expr();
  while (1) {
    if (accept("<<")) {
      binary1(type); /* pop {r1}; lsl r0, r1, r0 */
      type = binary2(additive_expr(), 8, "\x04\x10\x9d\xe4\x11\x00\xa0\xe1");
    }
    else if (accept(">>")) {
      binary1(type); /* pop {r1}; asr r0, r1, r0 */
      type = binary2(additive_expr(), 8, "\x04\x10\x9d\xe4\x51\x00\xa0\xe1");
    }
    else
      return type;
  }
}

/*
 * relational-expr:
 *         shift-expr
 *         relational-expr <= shift-expr
 */
int relational_expr()
{
  int type = shift_expr();
  while (accept("<=")) {
    binary1(type);
    /* pop {r1}; cmp r1, r0; movle r0, #1; movgt r0, #0 */
    type = binary2(shift_expr(),
		   16, "\x04\x10\x9d\xe4\x00\x00\x51\xe1\x01\x00\xa0\xd3\x00\x00\xa0\xc3");
  }
  return type;
}

/*
 * equality-expr:
 *         relational-expr
 *         equality-expr == relational-expr
 *         equality-expr != relational-expr
 */
int equality_expr()
{
  int type = relational_expr();
  while (1) {
    if (accept("==")) {
      binary1(type);
      /* pop {r1}; cmp r1, r0; moveq r0, #1; movne r0, #0 */
      type = binary2(relational_expr(),
		     16, "\x04\x10\x9d\xe4\x00\x00\x51\xe1\x01\x00\xa0\x03\x00\x00\xa0\x13");
    }
    else if (accept("!=")) {
      binary1(type);
      /* pop {r1}; cmp r1, r0; moveq r0, #0; movne r0, #1 */
      type = binary2(relational_expr(),
		     16, "\x04\x10\x9d\xe4\x00\x00\x51\xe1\x00\x00\xa0\x03\x01\x00\xa0\x13");
    }
    else
      return type;
  }
}

/*
 * bitwise-and-expr:
 *         equality-expr
 *         bitwise-and-expr & equality-expr
 */
int bitwise_and_expr()
{
  int type = equality_expr();
  while (accept("&")) {
    binary1(type); /* pop {r1}; and r0, r1, r0 */
    type = binary2(equality_expr(), 8, "\x04\x10\x9d\xe4\x00\x00\x01\xe0");
  }
  return type;
}

/*
 * bitwise-or-expr:
 *         bitwise-and-expr
 *         bitwise-and-expr | bitwise-or-expr
 */
int bitwise_or_expr()
{
  int type = bitwise_and_expr();
  while (accept("|")) {
    binary1(type); /* pop {r1}; orr r0, r1, r0 */
    type = binary2(bitwise_and_expr(), 8, "\x04\x10\x9d\xe4\x00\x00\x81\xe1");
  }
  return type;
}

/*
 * expression:
 *         bitwise-or-expr
 *         bitwise-or-expr = expression
 */
int expression()
{
  int type = bitwise_or_expr();
  if (accept("=")) {
    be_push();
    stack_pos = stack_pos + 1;
    promote(expression());
    if (type == 2)
      emit(8, "\x04\x10\x9d\xe4\x00\x00\x81\xe5"); /* pop {r1}; str r0, [r1] */
    else
      emit(8, "\x04\x10\x9d\xe4\x00\x00\xc1\xe5"); /* pop {r1}; strb r0, [r1] */
    stack_pos = stack_pos - 1;
    type = 3;
  }
  return type;
}

/*
 * type-name:
 *     char *
 *     int
 */
void type_name()
{
  get_token();
  while (accept("*")) {
  }
}

/*
 * statement:
 *     { statement-list-opt }
 *     type-name identifier ;
 *     type-name identifier = expression;
 *     if ( expression ) statement
 *     if ( expression ) statement else statement
 *     while ( expression ) statement
 *     return ;
 *     expr ;
 */
void statement()
{
  int p1;
  int p2;
  if (accept("{")) {
    int n = table_pos;
    int s = stack_pos;
    while (accept("}") == 0)
      statement();
    table_pos = n;
    be_pop(stack_pos - s);
    stack_pos = s;
  }
  else if (peek("char") | peek("int")) {
    type_name();
    sym_declare(token, 'L', stack_pos);
    get_token();
    if (accept("="))
      promote(expression());
    expect(";");
    be_push();
    stack_pos = stack_pos + 1;
  }
  else if (accept("if")) {
    expect("(");
    promote(expression());
    emit(8, "\x00\x00\x10\xe1...\x0a"); /* tst r0, r0; beq ... */
    p1 = codepos;
    expect(")");
    statement();
    emit(4, "...\xea"); /* b ... */
    p2 = codepos;
    save_imm24(code + p1 - 4, encode_pc_relative(codepos - p1 + 4));
    if (accept("else"))
      statement();
    save_imm24(code + p2 - 4, encode_pc_relative(codepos - p2 + 4));
  }
  else if (accept("while")) {
    expect("(");
    p1 = codepos;
    promote(expression());
    emit(8, "\x00\x00\x10\xe1...\x0a"); /* tst r0, r0; beq ... */
    p2 = codepos;
    expect(")");
    statement();
    emit(4, "...\xea"); /* b ... */
    save_imm24(code + codepos - 4, encode_pc_relative(p1 - codepos + 4));
    save_imm24(code + p2 - 4, encode_pc_relative(codepos - p2 + 4));
  }
  else if (accept("return")) {
    if (peek(";") == 0)
      promote(expression());
    expect(";");
    be_pop(stack_pos);
    emit(4, "\x1e\xff\x2f\xe1"); /* bx lr */
  }
  else {
    expression();
    expect(";");
  }
}

/*
 * program:
 *     declaration
 *     declaration program
 *
 * declaration:
 *     type-name identifier ;
 *     type-name identifier ( parameter-list ) ;
 *     type-name identifier ( parameter-list ) statement
 *
 * parameter-list:
 *     parameter-declaration
 *     parameter-list, parameter-declaration
 *
 * parameter-declaration:
 *     type-name identifier-opt
 */
void program()
{
  int current_symbol;
  while (token[0]) {
    type_name();
    current_symbol = sym_declare_global(token);
    get_token();
    if (accept(";")) {
      sym_define_global(current_symbol);
      emit(4, "\x00\x00\x00\x00");
    }
    else if (accept("(")) {
      int n = table_pos;
      number_of_args = 0;
      while (accept(")") == 0) {
	number_of_args = number_of_args + 1;
	type_name();
	if (peek(")") == 0) {
	  sym_declare(token, 'A', number_of_args);
	  get_token();
	}
	accept(","); /* ignore trailing comma */
      }
      if (accept(";") == 0) {
	sym_define_global(current_symbol);
	statement();
	emit(4, "\x1e\xff\x2f\xe1"); /* bx lr */
      }
      table_pos = n;
    }
    else
      error();
  }
}

int main1()
{
  code_offset = 32768; /* 0x8000 */
  be_start();
  nextc = getchar();
  get_token();
  program();
  be_finish();
  return 0;
}
