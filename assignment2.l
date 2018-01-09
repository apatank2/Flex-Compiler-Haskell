%{
#define stack_size 100
static int sp, stack [stack_size], stack1 [stack_size];
static int sp1;
static void push (int i) {
	//printf("Push => %d \n", i);
   if (++sp<stack_size) stack[sp]= i;
   else {printf ("error: stack overflow\n"); exit(1);}
}
static void push1 (int i) {
   if (++sp1<stack_size) stack1[sp1]= i;
   else {printf ("error: stack overflow\n"); exit(1);}
}
static int pop (void) {
   if (sp>=0) return stack[sp--];
   else {printf ("error: stack underflow\n"); exit(1);}
}
static int pop1 (void) {
   if (sp1>=0) return stack1[sp1--];
   else {printf ("error: stack underflow\n"); exit(1);}
}
static int peek1()
{
	return stack1[sp1];
}
static int peek()
{
	return stack[sp];
}

static void operation(int op)
{
	if(op==1){push (pop() && pop());}
	else if(op==2){int rhs= pop(); push (pop() || rhs);}
	else if(op==3){push (pop() * pop());}//ND
	else if(op==4){int rhs= pop(); push (pop() <= rhs);}//ND
}
static int sp1Value()
{
	return sp1;
}

static int converOperatorToInt(char op)
{
	if(op=='&')
		return 1;
	if(op=='|')
		return 2;
	if(op=='~')
		return 3;
	if(op=='!')
		return 4;
	if(op=='(')
		return 5;
	if(op==')')
		return 6;
}

static int priority(char op)
{
	if(op=='&' || op=='|' || op=='!')return 1;
	if(op=='~')return 2;
	if(op=='(' || op==')')return 3;
}
static int priorityInt(int op)
{
	if(op==1 || op==2 || op==4)return 1;
	if(op==3)return 2;
	if(op==5 || op==6)return 3;
}


static void checkPriority(char op)
{	
	if(op==';')
	{	while (1)
		{	
			if(sp1Value() == 0)
				break;
			operation(pop1());
		}
	}
	else if(sp1Value() == 0)
	{
		push1(converOperatorToInt(op));	//done
		//printf("Zero value \n");
	}
	else if(op=='(')
	{
		push1(converOperatorToInt(op));
	}
	else if(op==')')
	{
		operation(pop1());

	}
	else if(priority(op) <= priorityInt(peek1()))
	{
	 	operation(pop1());
	 	push1(converOperatorToInt(op));
	}
	else if(priorityInt(peek1()) < priority(op))
	{
		push1(converOperatorToInt(op));
	}
}


%}
%%
"True"                   {push (1);}
"False"                   {push (0);}
"("                      {char myChar = yytext[0];checkPriority(myChar);}
")"                      {checkPriority(yytext[0]);}
"/\\"                      {checkPriority('&');}
"\\/"                      {checkPriority('|');}
"->"                      {checkPriority('!');}
"not"                      {checkPriority('~');}
;                        {checkPriority(yytext[0]); int temp = pop(); if(temp==1) {printf ("True \n");}else {printf("False \n");}}
[ \t\n]                  ;
[^-0-9+*/; \t\n]+        {ECHO; printf (" unexpected\n");}
%%                        /* C code */
int main (void) {sp= 0; yylex(); return 0;}
int yywrap (void) {return 1;}
