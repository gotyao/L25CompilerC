#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <windows.h>

#define bool int
#define true 1
#define false 0

#define norw 20          /* 保留字个数 */
#define txmax 100        /* 符号表容量 */
#define nmax 6           /* 数字的最大位数 */
#define strmax 100       /* 字符串常量的最大位数 */
#define al 50            /* 标识符的最大长度 */
#define maxerr 0         /* 允许的最多错误数 */
#define amax 65538       /* 地址上界*/
#define levmax 3         /* 最大允许过程嵌套声明层数*/
#define cxmax 2000       /* 最多的虚拟机代码数 */
#define stacksize 500    /* 运行时数据栈元素最多为500个 */
#define heapsize 10000   /* 运行时堆最大为10000个单位 */
#define staticsize 10000 /* 运行时静态区最大为10000个单位 */

 /* 符号 */
enum symbol {
	nul, ident, number, plus, minus,
	times, slash, eql, neq, lss, 
    leq, gtr, geq, lparen, rparen, 
    comma, semicolon, lbrace, rbrace, becomes,
    amper, lbracket, rbracket, string, period,
    programsym, funcsym, mainsym, letsym, returnsym,
	ifsym, elsesym, whilesym, inputsym, outputsym,
    allocsym, freesym, strsym,
    mapsym, setsym, insertsym, deletesym, findsym,
    traversesym, sizesym,
};
#define symnum 45

/* 符号表中的类型 */
enum object {
    program,    /* 一个程序只有一个的program类型，没什么实际作用 */
	variable,   /* 变量类型，包含普通变量、字符串变量、指针变量、集合变量、字典变量 */
	function,   /* 函数类型 */
};

/* 值类型 (用于类型检查) */
enum ValType {
    type_int,  /* 整型类 */
    type_str,  /* 字符串类 */
    type_ptr,  /* 指针类 */
};

/* 虚拟机代码指令 */
enum fct {
	lit, opr, lod,
	sto, cal, ini,
	jmp, jpc, addr,
    loda, stoa, alloc,
    fre, 
    cpystr, litstr,
    refinc, refdec,
    concat, repeat,
    set_init, map_init,
    tree_free,
    insert,
    delete, find,
    traverse, 
    size,
};
#define fctnum 27

/* 虚拟机代码结构 */
struct instruction {
	enum fct f; /* 虚拟机代码指令 */
	int l;      /* 引用层与声明层的层次差 */
	int a;      /* 根据f的不同而不同 */
};

bool listswitch;                    /* 显示虚拟机代码与否 */
bool tableswitch;                   /* 显示符号表与否 */
char ch;                            /* 存放当前读取的字符，getch 使用 */
enum symbol sym;                    /* 当前的符号 */
char id[al + 1];                    /* 当前ident，多出的一个字节用于存放0 */
int num;                            /* 当前number */
int cc, ll;                         /* getch使用的计数器，cc表示当前字符(ch)的位置 */
int cx;                             /* 虚拟机代码指针, 取值范围[0, cxmax-1]*/
char line[101];                     /* 读取行缓冲区 */
char str_buf[strmax + 1];           /* 字符串缓冲区 */
char a[al + 1];                     /* 临时符号，多出的一个字节用于存放0 */
struct instruction code[cxmax];     /* 存放虚拟机代码的数组，空递增 */
char word[norw][al];                /* 保留字 */
enum symbol wsym[norw];             /* 保留字对应的符号值 */
enum symbol ssym[256];              /* 单字符的符号值 */
char mnemonic[fctnum][15];          /* 虚拟机代码指令名称 */
bool statbegsys[symnum];            /* 表示语句开始的符号集合 */
bool facbegsys[symnum];             /* 表示因子开始的符号集合 */
int return_jmp_addresses[cxmax];    /* 存储return语句生成的指令地址 */
int num_return_jmps;                /* 存储return语句数量 */
int static_str_addresses[strmax];   /* 存储静态字面量在s数组中的起始地址 */
int current_static_offset;          /* 记录当前静态区偏移 */
char const_str[strmax][strmax + 1]; /* 存储字符串字面量 */
int const_str_num;                  /* 存储字符串字面量个数 */

/* 符号表结构 */
struct tablestruct {
	char name[al];	           /* 名字 */
	enum object kind;          /* 类型：program, variable或procedure */
	int level;                 /* 所处层 */
	int adr;                   /* 地址 */
	int size;                  /* 需要分配的数据区空间 */
	int num_params;            /* 参数个数 ，仅procedure使用 */
    bool is_ptr;           /* 是否为指针变量 */
    bool is_str;               /* 是否为字符串变量 */
    bool is_set;               /* 是否为数组变量 */
    bool is_map;               /* 是否为字典变量 */
    enum ValType return_type;  /* 返回值类型 ，仅procedure使用 */
};

struct tablestruct table[txmax]; /* 符号表，满递增 */
int tx;                          /* 符号表当前尾指针 */

FILE* fin;      /* 输入源文件 */
FILE* ftable;	/* 输出符号表 */
FILE* fcode;    /* 输出虚拟机代码 */
FILE* foutput;  /* 输出文件及出错示意（如有错）、各行对应的生成代码首地址（如无错） */
FILE* fresult;  /* 输出执行结果 */
char fname[al]; /* 输入文件名 */
int err;        /* 错误计数器 */

#pragma region 函数声明

/* 初始化 */
void init();

/* 词法分析 */
void getch();
void getsym();

/* 错误报告 */
void error(int n);
void test(bool* s1, bool* s2, int n);

/* 数组运算 */
int inset(int e, bool* s);

/* 语法分析 */
void parse_program(int lev, int* ptx, bool* fsys);
void parse_func_def(int lev, int* ptx, bool* fsys);
void parse_stmt_list(int lev, int* ptx, bool* fsys, int* pdx, bool is_func, int func_tx);
void parse_stmt(int lev, int* ptx, bool* fsys, int* pdx, bool is_func, int func_tx);
void parse_bool_expr(int lev, int* ptx, bool* fsys);
enum ValType parse_expr(int lev, int* ptx, bool* fsys);
enum ValType parse_term(int lev, int* ptx, bool* fsys);
enum ValType parse_factor(int lev, int* ptx, bool* fsys);

/* 处理符号表 */
void enter(enum object k, int* ptx, int lev, int* pdx);
int position(char* id, int tx);
void listtable(int *ptx);

/* 代码生成函数 */
void gen(enum fct x, int y, int z);
void listcode(int cx0);
void listall();

/* 指针辅助函数 */
int try_alloc(int size, int* avail);
void free_heap_block(int start_ptr, int* s, int* avail);

/* 字符串辅助函数 */
void str2int(char* c_str, int* int_arr);
void int2str(int* int_arr, char* c_str);
int get_str_len(int* int_arr);

/* 集合辅助函数 */
int alloc_set_node(int* s, int* avail, int l_ptr, int val, int r_ptr);
void free_set_node(int node_ptr, int* s, int* avail);
void collect_set_values(int node_ptr, int* s, int* index);

/* 字典辅助函数 */
int alloc_map_node(int* s, int* avail, int l_ptr, int key, int val, int r_ptr);
void free_map_node(int node_ptr, int* s, int* avail);
void collect_map_values(int node_ptr, int* s, int* index);

/* 虚拟机指令执行 */
void interpret();
int base(int l, int* s, int b);

#pragma endregion

/* 主程序开始 */
int main() {
    bool nxtlev[symnum];

    printf("Input L25 file?   ");
    scanf("%s", fname);		/* 输入文件名 */

    if ((fin = fopen(fname, "r")) == NULL) {
        printf("Can't open the input file!\n");
        exit(1);
    }

    ch = fgetc(fin);
    if (ch == EOF) {
        printf("The input file is empty!\n");
        fclose(fin);
        exit(1);
    }
    rewind(fin);

    if ((foutput = fopen("foutput.txt", "w")) == NULL) {
        printf("Can't open the output file!\n");
        exit(1);
    }

    if ((ftable = fopen("ftable.txt", "w")) == NULL) {
        printf("Can't open ftable.txt file!\n");
        exit(1);
    }

    printf("List object codes?(Y/N)");	/* 是否输出虚拟机代码 */
    scanf("%s", fname);
    listswitch = (fname[0] == 'y' || fname[0] == 'Y');

    printf("List symbol table?(Y/N)");	/* 是否输出符号表 */
    scanf("%s", fname);
    tableswitch = (fname[0] == 'y' || fname[0] == 'Y');

    init();		/* 初始化 */
    err = 0;
    cc = ll = tx = cx = num_return_jmps = 0;
    ch = ' ';
    /* 第0个是空字符串，初始化 */
    const_str_num = 1; 
    static_str_addresses[0] = 0;  
    current_static_offset = 1;
    for (int i = 0; i < strmax; i++) {
        memset(const_str, 0, sizeof(char) * (strmax + 1));
    }

    /* 编译程序主体 */
    getsym();
    memset(nxtlev, false, sizeof(bool) * symnum);
    nxtlev[rbrace] = true;
    parse_program(0, &tx, nxtlev);

    /* 没有错误，执行代码 */
    if (err == 0) {
        printf("\n===Parsing success!===\n");
        fprintf(foutput, "\n===Parsing success!===\n");

        if ((fcode = fopen("fcode.txt", "w")) == NULL) {
            printf("Can't open fcode.txt file!\n");
            exit(1);
        }

        if ((fresult = fopen("fresult.txt", "w")) == NULL) {
            printf("Can't open fresult.txt file!\n");
            exit(1);
        }

        listall();	 /* 输出所有代码 */
        fclose(fcode);

        interpret();	/* 调用解释执行程序 */
        fclose(fresult);
    }
    else {
        printf("\n%d errors in L25 program!\n", err);
        fprintf(foutput, "\n%d errors in L25 program!\n", err);
    }

    fclose(ftable);
    fclose(foutput);
    fclose(fin);

    return 0;
}

/* 初始化 */
void init() {
    int i;

    /* 设置单字符符号 */
    for (i = 0; i <= 255; i++) {
        ssym[i] = nul;
    }
    ssym['+'] = plus;
    ssym['-'] = minus;
    ssym['*'] = times;
    ssym['/'] = slash;
    ssym['('] = lparen;
    ssym[')'] = rparen;
    ssym['='] = becomes;
    ssym[','] = comma;
    ssym[';'] = semicolon;
    ssym['{'] = lbrace;
    ssym['}'] = rbrace;
    ssym['&'] = amper;
    ssym['['] = lbracket;
    ssym[']'] = rbracket;
    ssym['.'] = period;

    /* 设置保留字名字,按照字母顺序，便于二分查找 */
    strcpy(&(word[0][0]), "alloc");
    strcpy(&(word[1][0]), "delete");
    strcpy(&(word[2][0]), "else");
    strcpy(&(word[3][0]), "find");
    strcpy(&(word[4][0]), "free");
    strcpy(&(word[5][0]), "func");
    strcpy(&(word[6][0]), "if");
    strcpy(&(word[7][0]), "input");
    strcpy(&(word[8][0]), "insert");
    strcpy(&(word[9][0]), "let");
    strcpy(&(word[10][0]), "main");
    strcpy(&(word[11][0]), "map");
    strcpy(&(word[12][0]), "output");
    strcpy(&(word[13][0]), "program");
    strcpy(&(word[14][0]), "return");
    strcpy(&(word[15][0]), "set");
    strcpy(&(word[16][0]), "size");
    strcpy(&(word[17][0]), "str");
    strcpy(&(word[18][0]), "traverse");
    strcpy(&(word[19][0]), "while");

    /* 设置保留字符号 */
    wsym[0] = allocsym;
    wsym[1] = deletesym;
    wsym[2] = elsesym;
    wsym[3] = findsym;
    wsym[4] = freesym;
    wsym[5] = funcsym;
    wsym[6] = ifsym;
    wsym[7] = inputsym;
    wsym[8] = insertsym;
    wsym[9] = letsym;
    wsym[10] = mainsym;
    wsym[11] = mapsym;
    wsym[12] = outputsym;
    wsym[13] = programsym;
    wsym[14] = returnsym;
    wsym[15] = setsym;
    wsym[16] = sizesym;
    wsym[17] = strsym;
    wsym[18] = traversesym;
    wsym[19] = whilesym;

    /* 设置指令名称 */
    strcpy(&(mnemonic[lit][0]), "lit");
    strcpy(&(mnemonic[opr][0]), "opr");
    strcpy(&(mnemonic[lod][0]), "lod");
    strcpy(&(mnemonic[sto][0]), "sto");
    strcpy(&(mnemonic[cal][0]), "cal");
    strcpy(&(mnemonic[ini][0]), "ini");
    strcpy(&(mnemonic[jmp][0]), "jmp");
    strcpy(&(mnemonic[jpc][0]), "jpc");
    strcpy(&(mnemonic[addr][0]), "addr");
    strcpy(&(mnemonic[loda][0]), "loda");
    strcpy(&(mnemonic[stoa][0]), "stoa");
    strcpy(&(mnemonic[alloc][0]), "alloc");
    strcpy(&(mnemonic[fre][0]), "fre");
    strcpy(&(mnemonic[cpystr][0]), "cpystr");
    strcpy(&(mnemonic[litstr][0]), "litstr");
    strcpy(&(mnemonic[refinc][0]), "refinc");
    strcpy(&(mnemonic[refdec][0]), "refdec");
    strcpy(&(mnemonic[concat][0]), "concat");
    strcpy(&(mnemonic[repeat][0]), "repeat");
    strcpy(&(mnemonic[set_init][0]), "set_init");
    strcpy(&(mnemonic[map_init][0]), "map_init");
    strcpy(&(mnemonic[tree_free][0]), "tree_free");
    strcpy(&(mnemonic[insert][0]), "insert");
    strcpy(&(mnemonic[delete][0]), "delete");
    strcpy(&(mnemonic[find][0]), "find");
    strcpy(&(mnemonic[traverse][0]), "traverse");
    strcpy(&(mnemonic[size][0]), "size");

    /* 设置符号集 */
    for (i = 0; i < symnum; i++) {
        statbegsys[i] = false;
        facbegsys[i] = false;
    }

    /* 设置语句开始符号集 */
    statbegsys[ident] = true;
    statbegsys[ifsym] = true;
    statbegsys[whilesym] = true;
    statbegsys[lbrace] = true;
    statbegsys[times] = true;
    statbegsys[inputsym] = true;
    statbegsys[outputsym] = true;
    statbegsys[letsym] = true;
    statbegsys[allocsym] = true;
    statbegsys[freesym] = true;

    /* 设置因子开始符号集 */
    facbegsys[ident] = true;
    facbegsys[number] = true;
    facbegsys[lparen] = true;
    facbegsys[times] = true;
    facbegsys[amper] = true;
    facbegsys[string] = true;
}

/* e是否在集合s内 */
int inset(int e, bool* s) {
    return s[e];
}

/*
 * 过滤空白符和注释，读取一个字符
 * 每次读一行，存入line缓冲区，line被getsym取空后再读一行
 * 被函数getsym调用
 */
void getch() {
    if (cc == ll) { /* 判断缓冲区中是否有字符，若无字符，则读入下一行字符到缓冲区中 */
        if (feof(fin)) {
            ch = EOF;
            return;
        }

        ll = 0;
        cc = 0;
        ch = ' ';

        memset(line, 0, sizeof(line));
        printf("%d ", cx);
        fprintf(foutput, "%d ", cx);

        while (ch != '\n') {
            if (EOF == fscanf(fin, "%c", &ch)) {
                line[ll] = 0;
                break;
            }

            printf("%c", ch);
            fprintf(foutput, "%c", ch);
            line[ll] = ch;
            ll++;
        }
    }
    ch = line[cc];
    cc++;
}

/* 词法分析，获取一个符号 */
void getsym() {
    int i, j, k;

    while (ch == ' ' || ch == '\n' || ch == '\t' || ch == '#') {	/* 过滤空格、换行和制表符 */
        if (ch == '#') {
            while (ch != '\n' && ch != EOF) {
                getch();
            }
        }
        getch();
    }

    if (ch == EOF) {
        sym = nul;
        return;
    }

    if (isalpha(ch)) { /* 当前的单词是标识符或是保留字 */
        k = 0;
        do {
            if (k < al)
            {
                a[k] = ch;
                k++;
            }
            getch();
        } while (isalnum(ch));
        a[k] = 0;
        strcpy(id, a);

        i = 0;
        j = norw - 1;
        do {    /* 搜索当前单词是否为保留字，使用二分法查找 */
            k = (i + j) / 2;
            if (strcmp(id, word[k]) <= 0)
                j = k - 1;
            if (strcmp(id, word[k]) >= 0)
                i = k + 1;
        } while (i <= j);

        if (i - 1 > j) /* 当前的单词是保留字 */
            sym = wsym[k];
        else /* 当前的单词是标识符 */
            sym = ident;
    }
    else if (isdigit(ch)) { /* 当前的单词是数字 */
        k = 0;
        num = 0;
        sym = number;
        do {
            if (k < nmax)
                num = 10 * num + (ch - '0');
            k++;
            getch();
        } while (isdigit(ch)); /* 获取数字的值 */
        k--;

        if (k > nmax) { /* 数字位数太多 */
            error(13);
            num = 0;
        }
    }
    else if (ch == '"') { /* 当前的单词是字符串字面量 */
        k = 0;
        getch();
        /* 读取至字符串末尾 */
        while (ch != '"') {
            str_buf[k++] = ch;
            getch();
            if (k >= strmax) { /* 字符串过长 */
                error(23);
            }
        }
        getch();
        str_buf[k] = '\0';

        /* 在静态存储区查找字符串字面量 */
        for (i = 0; i < const_str_num; i++) {
            if (strcmp(str_buf, const_str[i]) == 0) {
                break;
            }
        }

        /* 找不到，存入静态存储区 */
        if (i == const_str_num) {
            if (const_str_num == strmax) { /* 字符串字面量过多 */
                error(24);
            }
            else {
                strcpy(const_str[const_str_num], str_buf);
                const_str_num++;
            }
            static_str_addresses[i] = current_static_offset;
            current_static_offset = current_static_offset + k + 1;
        }

        num = static_str_addresses[i] + stacksize + heapsize;
        sym = string;
    }
    else {
        if (ch == '=') {    /* 检测赋值或相等符号 */
            getch();
            if (ch == '=') {
                sym = eql;
                getch();
            }
            else {
                sym = becomes;
            }
        }
        else if (ch == '!') { /* 检测不等符号 */
            getch();
            if (ch == '=') {
                sym = neq;
                getch();
            }
            else { /* 未识别的符号 */
                sym = nul;
                error(15);
            }
        }
        else if (ch == '<') {    /* 检测小于或小于等于符号 */
            getch();
            if (ch == '=') {
                sym = leq;
                getch();
            }
            else {
                sym = lss;
            }
        }
        else if (ch == '>') {    /* 检测大于或大于等于符号 */
            getch();
            if (ch == '=') {
                sym = geq;
                getch();
            }
            else {
                sym = gtr;
            }
        }
        else {
            sym = ssym[(int)ch];
            if (sym != nul) {
                getch();
            } 
            else { /* 未识别的符号 */
                error(15);
                getch();
            }
        }
    }

    // printf("sym = %d , id = %s\n", sym, id);
}

/* 
 * 出错处理，打印出错位置和错误编码
 *
 * n: 错误号
 */
void error(int n) {
    char space[81];
    memset(space, ' ', 81);

    space[cc - 1] = '\0'; /* 出错时当前符号已经读完，所以cc-1 */

    printf("**%s^%d\n", space, n);
    fprintf(foutput, "**%s^%d\n", space, n);

    err += 1;
    if (err > maxerr) {
        exit(1);
    }
}

/*
 * 测试当前符号是否合法
 *
 * 在语法分析程序的入口和出口处调用测试函数test，
 * 检查当前单词进入和退出该语法单位的合法性
 *
 * s1:	需要的单词集合
 * s2:	如果不是需要的单词，在某一出错状态时，
 *      可恢复语法分析继续正常工作的补充单词符号集合
 * n:  	错误号
 */
void test(bool* s1, bool* s2, int n) {
    if (!inset(sym, s1)) {
        error(n);
        /* 当检测不通过时，不停获取符号，直到它属于需要的集合或补救的集合 */
        while ((!inset(sym, s1)) && (!inset(sym, s2))){
            getsym();
        }
    }
}

/*
 * 编译程序主体
 * <program> = "program" <ident> "{" {<func_def>} "main" "{" <stmt_list> "}" "}"
 *
 * lev:    当前分程序所在层
 * ptx:    符号表当前尾指针
 * fsys:   当前模块后继符号集合
 */
void parse_program(int lev, int* ptx, bool* fsys) {
    int dx;                 /* 记录数据分配的相对地址 */
    int tx0;                /* 保留初始tx */
    int cx0;                /* 保留初始cx */
    bool nxtlev[symnum];    /* 在下级函数的参数中，符号集合均为值参，但由于使用数组实现，
                                传递进来的是指针，为防止下级函数改变上级函数的集合，开辟新的空间
                                传递给下级函数*/

    dx = 3;                 /* 三个空间用于存放静态链SL、动态链DL和返回地址RA  */
    gen(jmp, 0, 0);         /* 产生跳转指令，跳转位置未知暂时填0 */

    if (sym != programsym) {
        error(16);
    }
    getsym();

    /* 将program加入符号表 */
    enter(program, ptx, lev, &dx);
    getsym();
    tx0 = *ptx;		        /* 记录本层标识符的初始位置 */

    if (sym != lbrace) {
        error(17);
    }
    getsym();

    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
    nxtlev[funcsym] = true;
    nxtlev[mainsym] = true;
    nxtlev[rbrace] = true;

    /* 处理函数定义 */
    while (sym == funcsym) {
        getsym();
        if (sym != ident) {
            error(21);
        }
        enter(function, ptx, lev, &dx);
        int tx1 = *ptx;
        getsym();
        parse_func_def(lev + 1, ptx, nxtlev);

        /* 处理当前作用域内的所有字符串、字典、集合变量 */
        for (int i = tx1 + 1; i <= *ptx; i++) {
            if (table[i].kind == variable) {
                if (table[i].is_str) {
                    gen(lod, 0, table[i].adr);
                    gen(refdec, 0, 0); /* 递减引用计数 */
                    gen(opr, 0, 20);
                }
                else if (table[i].is_map || table[i].is_set) {
                    gen(lod, 0, table[i].adr);
                    gen(tree_free, 0, 0); /* 释放二叉树 */
                    gen(opr, 0, 20);
                }
            }
        }
        *ptx = tx1;
        test(nxtlev, 0, 18);
    }

    /* 处理main部分 */
    if (sym != mainsym) {
        error(19);
    }
    cx0 = cx;
    code[0].a = cx;	/* 把前面生成的跳转语句的跳转位置改成当前位置 */
    table[tx0].adr = cx;   /* 记录main代码的开始位置 */
    gen(ini, 0, dx);	  /* 生成指令，此指令执行时在数据栈中为被调用的过程开辟dx个单元的数据区 */

    getsym();
    parse_stmt_list(lev, ptx, fsys, &dx, false, tx0);

    /* 语句后继符号为} */
    memset(nxtlev, 0, sizeof(bool) * symnum);	/* 分程序没有补救集合 */
    test(fsys, nxtlev, 20);        /* 检测程序结尾正确性 */
    code[table[tx0].adr].a = dx;   /* 记录声明的变量个数 */
    table[tx0].size = dx;          /* 声明部分中每增加一条声明都会给dx增加1，声明部分已经结束，dx就是当前过程数据的size */

    printf("\n");

    listtable(ptx); /* 输出符号表 */

    gen(opr, 0, 0);	 /* 每个过程出口都要使用的释放数据段指令 */
    listcode(cx0);   /* 输出main程序生成的代码 */
}

/*
 * 编译函数定义
 * <func_def> = "func" <ident> "(" [<param_list>] ")" "{" <stmt_list> "return" <expr> ";" "}"
 *
 * lev:    当前分程序所在层
 * ptx:    符号表当前尾指针
 * fsys:   当前模块后继符号集合
 */
void parse_func_def(int lev, int* ptx, bool* fsys) {
    int dx;                 /* 记录数据分配的相对地址 */
    int tx0;                /* 保留初始tx */
    int cx0;                /* 保留初始cx */
    int num_params = 0;     /* 记录参数数量 */
    bool nxtlev[symnum];    /* 在下级函数的参数中，符号集合均为值参，但由于使用数组实现，
                                传递进来的是指针，为防止下级函数改变上级函数的集合，开辟新的空间
                                传递给下级函数*/

    dx = 3;               /* 三个空间用于存放静态链SL、动态链DL和返回地址RA  */
    tx0 = *ptx;		      /* 记录本层标识符的初始位置 */
    table[tx0].adr = cx;  /* 记录当前层代码的开始位置 */
    gen(jmp, 0, 0);       /* 产生跳转指令，跳转位置未知暂时填0 */

    if (sym != lparen) {
        error(7);
    }
    getsym();
    memcpy(nxtlev, fsys, sizeof(bool) * symnum);

    /* 处理函数参数 */
    if (sym != rparen) {
        bool param_fsys[symnum];
        memcpy(param_fsys, fsys, sizeof(bool) * symnum);
        param_fsys[comma] = true;
        param_fsys[rparen] = true;

        /* 参数为指针 */
        if (sym == times) {
            getsym();
            if (sym != ident) {
                error(32);
            }
            enter(variable, ptx, lev, &dx);
            table[*ptx].is_ptr = true;
            num_params++;
            getsym();
        }
        /* 参数为字符串 */
        else if (sym == strsym) {
            getsym();
            if (sym != ident) {
                error(32); 
            }
            enter(variable, ptx, lev, &dx);
            table[*ptx].is_str = true;
            num_params++;
            getsym();
        }
        /* 参数为整型 */
        else if (sym == ident) {
            enter(variable, ptx, lev, &dx);
            num_params++;
            getsym();
        }
        else {
            error(32);
        }
        
        while (sym == comma) {
            getsym();
            /* 参数为指针 */
            if (sym == times) {
                getsym();
                if (sym != ident) {
                    error(32);
                }
                enter(variable, ptx, lev, &dx);
                table[*ptx].is_ptr = true;
                num_params++;
                getsym();
            }
            /* 参数为字符串 */
            else if (sym == strsym) {
                getsym();
                if (sym != ident) {
                    error(32); 
                }
                enter(variable, ptx, lev, &dx);
                table[*ptx].is_str = true;
                num_params++;
                getsym();
            }
            /* 参数为整型 */
            else if (sym == ident) {
                enter(variable, ptx, lev, &dx);
                num_params++;
                getsym();
            }
            else {
                error(32);
            }
        }
    }

    if (sym != rparen) {
        error(8);
    }
    
    table[tx0].num_params = num_params;  /* 将函数参数个数填入符号表 */
    cx0 = cx;
    code[table[tx0].adr].a = cx;	/* 把前面生成的跳转语句的跳转位置改成当前位置 */
    gen(ini, 0, dx);	            /* 生成指令，此指令执行时在数据栈中为被调用的过程开辟dx个单元的数据区 */
    /* 生成压入参数的指令 */
    for (int i = num_params; i >= 1; i--) {
        gen(lod, 0, -i);
        gen(sto, 0, (num_params - i) + 3);

        /* 字符串还需要增加引用计数，否则可能被提前释放 */
        if (table[tx0 + 1 + (num_params - i)].is_str) {
            gen(lod, 0, (num_params - i) + 3); /* 将参数地址再次压栈 */
            gen(refinc, 0, 0);    /* 增加引用计数 */
            gen(opr, 0, 20);      /* 弹出栈顶的重复地址 */
        }
    }

    /* 处理函数主体 */
    getsym();
    parse_stmt_list(lev, ptx, fsys, &dx, true, tx0);
    /* 函数内必须有返回语句 */
    if (num_return_jmps == 0) {
        error(35);
    }
    /* 回填所有 return 语句的跳转地址 */
    for (int i = 0; i < num_return_jmps; i++) {
        code[return_jmp_addresses[i]].a = cx;
    }
    num_return_jmps = 0; /* 重置计数器，为下一个函数做准备 */
    gen(opr, num_params, 0);

    code[table[tx0].adr + 1].a = dx;    /* 记录声明的变量个数 */
    table[tx0].size = dx;	            /* 每增加一条声明都会给dx增加1，dx就是当前过程数据的size */
    for (int i = table[tx0].adr; i < cx; i++) {
        if (code[i].f == opr && code[i].a == 0) {
            code[i].l = num_params;
        }
    }

    listcode(cx0);  /* 输出本分程序生成的代码 */
}

/*
 * 编译语句串
 * <stmt_list> = <stmt> ";" { <stmt> ";" }（实际包括两端大括号）
 *
 * lev:     当前分程序所在层
 * ptx:     符号表当前尾指针
 * fsys:    当前模块后继符号集合
 * pdx:     dx为当前应分配的变量的相对地址，分配后要增加1
 * is_func：句尾是否需要包含return语句
 * func_tx: 函数名在符号表内的位置，用于处理返回值
 */
void parse_stmt_list(int lev, int* ptx, bool* fsys, int* pdx, bool is_func, int func_tx) {
    bool nxtlev[symnum];
    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
    nxtlev[semicolon] = true;
    if (sym != lbrace) {
        error(17);
    }
    else {
        getsym();
    }

    int tx1 = *ptx;

    while (inset(sym, statbegsys)) {
        parse_stmt(lev, ptx, nxtlev, pdx, is_func, func_tx);
    }

    /* 处理返回语句 */
    if (sym == returnsym) {
        if (is_func) {
            getsym();
            enum ValType expr_type = parse_expr(lev, ptx, nxtlev);
            if (expr_type == type_str) {
                /* 如果返回字符串，增加其引用计数，避免提前释放 */
                gen(refinc, 0, 0);
            }
            /* 在有效的函数内确定函数的返回值类型 */
            if (func_tx != -1) {
                table[func_tx].return_type = expr_type;
            }
            if (sym != semicolon) {
                error(2);
            }
            /* 生成跳转指令并保存，之后统一回填为函数末尾的返回指令 */
            else {
                gen(jmp, 0, 0);
                return_jmp_addresses[num_return_jmps++] = cx - 1;
                while (sym != rbrace && sym != nul) {
                    getsym();
                }
            }
        }
        else {
            error(34);
        }
    }

    /* 处理当前作用域内的所有字符串、字典、集合变量 */
    for (int i = tx1 + 1; i <= *ptx; i++) {
        if (table[i].kind == variable) {
            if (table[i].is_str) {
                gen(lod, 0, table[i].adr);
                gen(refdec, 0, 0); /* 递减引用计数 */
                gen(opr, 0, 20);
            }
            else if (table[i].is_map || table[i].is_set) {
                gen(lod, 0, table[i].adr);
                gen(tree_free, 0, 0); /* 释放二叉树 */
                gen(opr, 0, 20);
            }
        }
    }

    /* 检测程序结尾正确性 */
    if (sym == rbrace) {
        getsym();
    } 
    else {
        error(20);
    }
    listtable(ptx); /* 输出符号表 */
    *ptx = tx1;
}

/*
 * 编译语句
 * <stmt> = <declare_stmt> | <assign_stmt> | <if_stmt> | <while_stmt> | <input_stmt> | 
 *          <output_stmt> | <func_call> | <alloc_stmt> | <free_stmt> | <return_stmt> | <update_stmt>
 *
 * lev:     当前分程序所在层
 * ptx:     符号表当前尾指针
 * fsys:    当前模块后继符号集合
 * pdx:     dx为当前应分配的变量的相对地址，分配后要增加1
 * is_func：句尾是否需要包含return语句，用于if或while的stmt_list内
 * func_tx: 函数名在符号表内的位置，用于处理返回值
 */
void parse_stmt(int lev, int* ptx, bool* fsys, int* pdx, bool is_func, int func_tx) {
    int i, cx1, cx2;
    bool nxtlev[symnum];
    enum ValType expr_type;

    /* 先要区分是赋值语句还是函数调用 */
    if (sym == ident) {
        i = position(id, *ptx);
        if (i == 0) {
            error(3);
        }

        getsym();

        /* 赋值语句 */
        if (sym == becomes) {
            if (table[i].kind == variable) {
                getsym();
                memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                expr_type = parse_expr(lev, ptx, nxtlev);	/* 处理赋值符号右侧表达式 */

                if (i != 0) {
                    /* 字符串变量赋值 */
                    if (table[i].is_str) {
                        if (expr_type != type_str) {
                            error(31);
                        }
                        gen(cpystr, 0, 0);
                        /* 处理旧值地址的引用计数器 */
                        gen(lod, lev - table[i].level, table[i].adr);
                        gen(refdec, 0, 0);
                        gen(opr, 0, 20); 
                        /* 存储新值，由于cpystr已经处理计数器，无需手动处理 */
                        gen(sto, lev - table[i].level, table[i].adr);
                    } 
                    /* 指针变量赋值 （非字符串指针）*/
                    else if (table[i].is_ptr) {
                        /* 指针可以赋值为地址或0 */
                        if (expr_type != type_ptr && expr_type != type_int) {
                            error(31);
                        }
                        gen(sto, lev - table[i].level, table[i].adr);
                    }
                    /* 普通整型变量赋值 */
                    else if (!table[i].is_set && !table[i].is_map) {
                        if (expr_type != type_int) {
                            error(31);
                        }
                        gen(sto, lev - table[i].level, table[i].adr);
                    }
                    /* 字典变量和集合变量不能赋值 */
                    else {
                        error(44);
                    }
                }
                else {
                    error(3);
                }
            }
            else {
                error(4);
                i = 0;
            }

            if (sym != semicolon) {
                error(2);
            }

            getsym();
        }
        /* 函数调用 */
        else if (sym == lparen) {
            if (table[i].kind == function) {
                if (table[i].num_params != 0) {
                    /* 处理参数列表 */
                    int param_cnt = 0;
                    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                    nxtlev[comma] = true;
                    nxtlev[rparen] = true;
                    getsym();
                    parse_expr(lev, ptx, nxtlev);
                    param_cnt++;
                    while (sym == comma) {
                        getsym();
                        param_cnt++;
                        parse_expr(lev, ptx, nxtlev);
                    }

                    if (param_cnt != table[i].num_params) {
                        error(22);
                    }
                }
                else {
                    getsym();
                }

                if (sym != rparen) {
                    error(8);
                }
                gen(cal, lev - table[i].level, table[i].adr);

                getsym();
                if (sym != semicolon) {
                    error(2);
                }
                getsym();
            }
            else {
                error(5);
                getsym();
            }
        }
        /* 为解引用赋值的语句 或者 字典的赋值 */
        else if (sym == lbracket) {
            if (table[i].kind == variable) {
                /* 为解引用赋值的语句 */
                if (table[i].is_ptr || table[i].is_str) {
                    gen(lod, lev - table[i].level, table[i].adr); /* 读取指针的值到栈顶 */
                    getsym();
                    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                    nxtlev[rbracket] = true;
                    expr_type = parse_expr(lev, ptx, nxtlev);	/* 处理[]内表达式 */
                    if (expr_type != type_int) {
                        error(30);
                    }
                    if (sym != rbracket) {
                        error(39);
                    }
                    else {
                        getsym();
                    }
                    if (sym != becomes) {
                        error(38);
                    }
                    gen(opr, 0, 17);
                    getsym();
                    nxtlev[rbracket] = false;
                    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                    expr_type = parse_expr(lev, ptx, nxtlev);	/* 处理赋值符号右侧表达式 */
                    /* 解引用赋值只能是整数 */
                    if (expr_type != type_int) {
                        error(31);
                    }

                    if (i != 0) {
                        /* expression将执行一系列指令，但最终结果将会保存在栈顶，执行stoa命令完成赋值 */
                        gen(stoa, 0, 0);
                    }
                    else {
                        error(3);
                    }
                }
                /* 字典的赋值 */
                else if (table[i].is_map) {
                    getsym();
                    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                    nxtlev[rbracket] = true;
                    expr_type = parse_expr(lev, ptx, nxtlev);	/* 处理[]内表达式 */
                    if (expr_type != type_int) {
                        error(46);
                    }
                    if (sym != rbracket) {
                        error(39);
                    }
                    else {
                        getsym();
                    }
                    if (sym != becomes) {
                        error(38);
                    }
                    getsym();
                    nxtlev[rbracket] = false;
                    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                    expr_type = parse_expr(lev, ptx, nxtlev);	/* 处理赋值符号右侧表达式 */
                    /* 字典赋值只能是整数 */
                    if (expr_type != type_int) {
                        error(31);
                    }

                    if (i != 0) {
                        gen(insert, lev - table[i].level, table[i].adr); 
                    }
                    else {
                        error(3);
                    }
                }
                else {
                    error(4);
                }
            }
            else {
                error(4);
            }

            if (sym != semicolon) {
                error(2);
            }

            getsym();
        }
        /* 字典或集合变量操作 */
        else if (sym == period) {
            if (table[i].kind != variable || !(table[i].is_set || table[i].is_map)) {
                error(45);
            }
            else {
                getsym();
                /* 插入操作 */
                if (sym == insertsym) {
                    getsym();
                    if (sym != lparen) {
                        error(7);
                    }
                    getsym();
                    if (table[i].is_set) {
                        expr_type = parse_expr(lev, ptx, nxtlev);
                        if (expr_type != type_int) {
                            error(46);
                        }
                    }
                    else {
                        /* 处理key */
                        nxtlev[comma] = true;
                        nxtlev[rparen] = true;
                        expr_type = parse_expr(lev, ptx, nxtlev);
                        if (expr_type != type_int) {
                            error(46);
                        }
                        if (sym != comma) {
                            error(50);
                        }
                        else {
                            getsym();
                        }
                        /* 处理value */
                        expr_type = parse_expr(lev, ptx, nxtlev);
                        if (expr_type != type_int) {
                            error(46);
                        }
                    }
                    if (sym != rparen) {
                        error(8);
                    }
                    getsym();
                    gen(insert, lev - table[i].level, table[i].adr);
                }
                /* 删除操作 */
                else if (sym == deletesym) {
                    getsym();
                    if (sym != lparen) {
                        error(7);
                    }
                    getsym();
                    expr_type = parse_expr(lev, ptx, nxtlev);
                    if (expr_type != type_int) {
                        error(46);
                    }
                    if (sym != rparen) {
                        error(8);
                    }
                    getsym();
                    gen(delete, lev - table[i].level, table[i].adr);
                }
                /* 其他操作 */
                else if (sym == findsym || sym == sizesym || sym == traversesym) {
                    error(47);
                }
                else {
                    error(48);
                }

                if (sym != semicolon) {
                    error(2);
                }
                getsym();
            }
        }
        else {
            test(fsys, 0, 33);
        }
    }
    /* 为解引用赋值的语句 */
    else if (sym == times) {
        getsym();
        if (sym != ident) {
            error(36);
        }

        i = position(id, *ptx);
        if (i == 0) {
            error(3);
        }
        if (table[i].kind != variable || ((!table[i].is_ptr) && (!table[i].is_str))) {
            error(36);
        }
        else {
            gen(lod, lev - table[i].level, table[i].adr);
        }

        getsym();
        if (sym != becomes) {
            error(38);
        }
        getsym();

        memcpy(nxtlev, fsys, sizeof(bool) * symnum);
        expr_type = parse_expr(lev, ptx, nxtlev);
        /* 解引用赋值只能是整数 */
        if (expr_type != type_int) {
            error(31);
        }

        gen(stoa, 0, 0);

        if(sym != semicolon) {
            error(2);
        }
        getsym();
    }
    /* 输入语句 */
    else if (sym == inputsym) {
        getsym();
        if (sym != lparen) {
            error(7);
        }
        else {
            do {
                getsym();
                if (sym == ident) {
                    i = position(id, *ptx);	/* 查找要读的变量 */
                }
                else {
                    i = 0;
                }

                /* input语句括号中的标识符应该是声明过的变量 */
                if (i == 0) {
                    error(3);
                }
                else {
                    /* 输入是字符串类型 */
                    if (table[i].is_str) {
                        /* 递减旧值引用计数*/
                        gen(lod, lev - table[i].level, table[i].adr);
                        gen(refdec, 0, 0); 
                        gen(opr, 0, 20);
                        /* 读取输入并压栈 */
                        gen(opr, 0, 16);
                        gen(sto, lev - table[i].level, table[i].adr);
                    }
                    /* 输入是整型 */
                    else {
                        /* 读取输入并压栈 */
                        gen(opr, 0, 15);
                        gen(sto, lev - table[i].level, table[i].adr);
                    }
                }
                getsym();
            } while (sym == comma);	/* 一条input语句可读多个变量 */
        }
        if (sym != rparen) {
            error(8);
        }
        else {
            getsym();
        }

        if (sym != semicolon) {
            error(2);
        }
        else {
            getsym();
        }
    }
    /* 输出语句 */
    else if (sym == outputsym) {
        getsym();
        if (sym == lparen) {
            do {
                getsym();
                memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                nxtlev[rparen] = true;
                nxtlev[comma] = true;
                expr_type = parse_expr(lev, ptx, nxtlev);	/* 调用表达式处理 */
                if (expr_type == type_str) { /* 如果是字符串，则输出字符串 */
                    gen(opr, 0, 13);
                }
                else { /* 否则输出整数或指针 */
                    gen(opr, 0, 12);
                }
            } while (sym == comma);  /* 一条output可输出多个变量的值 */
            /* 生成换行指令 */
            gen(opr, 0, 14);

            if (sym != rparen) {
                error(8);
            }
            else {
                getsym();
            }
            if (sym != semicolon) {
                error(2);
            }
            else {
                getsym();
            }
        }
    }
    /* if语句 */
    else if (sym == ifsym) {
        getsym();
        if (sym != lparen) {
            error(7);
        }
        getsym();
        memcpy(nxtlev, fsys, sizeof(bool) * symnum);
        nxtlev[rparen] = true;	/* 后继符号为) */
        parse_bool_expr(lev, ptx, nxtlev); /* 调用条件处理 */
        if (sym == rparen) {
            getsym();
        }
        else {
            error(7);
        }

        cx1 = cx;   /* 保存当前指令地址 */
        gen(jpc, 0, 0);	/* 生成条件跳转指令，跳转地址未知，暂时写0 */

        parse_stmt_list(lev, ptx, fsys, pdx, is_func, func_tx);

        cx2 = cx;   /* 保存if语句执行完毕的位置 */
        code[cx1].a = cx;	/* cx为else语句开始的位置 */

        if (sym == elsesym) {
            code[cx1].a++;
            gen(jmp, 0, 0); /* 跳转到else后 */
            getsym();
            parse_stmt_list(lev, ptx, fsys, pdx, is_func, func_tx);
            code[cx2].a = cx;   /* cx为else语句结束的位置 */
        }

        if (sym != semicolon) {
            error(2);
        }
        getsym();
    }
    /* while语句 */
    else if (sym == whilesym) {
        getsym();
        if (sym != lparen) {
            error(7);
        }
        cx1 = cx;	/* 保存判断条件操作的位置 */
        getsym();
        memcpy(nxtlev, fsys, sizeof(bool) * symnum);
        nxtlev[rparen] = true;	/* 后继符号为{ */
        parse_bool_expr(lev, ptx, nxtlev);	/* 调用条件处理 */
        if (sym == rparen) {
            getsym();
        }
        else {
            error(7);
        }
        cx2 = cx;	/* 保存循环体的结束的下一个位置 */
        gen(jpc, 0, 0);	/* 生成条件跳转，但跳出循环的地址未知，标记为0等待回填 */

        parse_stmt_list(lev, ptx, fsys, pdx, is_func, func_tx);

        gen(jmp, 0, cx1);	/* 生成条件跳转指令，跳转到前面判断条件操作的位置 */
        code[cx2].a = cx;	/* 回填跳出循环的地址 */

        if (sym != semicolon) {
            error(2);
        }
        getsym();
    }
    /* 声明语句 */
    else if (sym == letsym) {
        getsym();
        /* 声明一个整型变量 */
        if (sym == ident) {
            enter(variable, ptx, lev, pdx);
            i = *ptx;
            getsym();
        }
        /* 声明一个指针变量 */
        else if (sym == times) {
            getsym();
            if (sym != ident) {
                error(1);
            }
            enter(variable, ptx, lev, pdx);
            i = *ptx;
            table[i].is_ptr = true;
            table[i].size = 0;
            getsym();
        }
        /* 声明一个字符串变量 */
        else if (sym == strsym) {
            getsym();
            if (sym != ident) {
                error(1);
            }
            enter(variable, ptx, lev, pdx);
            i = *ptx;
            table[i].is_str = true;
            getsym();
        }
        /* 声明一个集合变量 */
        else if (sym == setsym) {
            getsym();
            if (sym != ident) {
                error(1);
            }
            enter(variable, ptx, lev, pdx);
            i = *ptx;
            table[i].is_set = true;
            getsym();
        }
        /* 声明一个字典变量 */
        else if (sym == mapsym) {
            getsym();
            if (sym != ident) {
                error(1);
            }
            enter(variable, ptx, lev, pdx);
            i = *ptx;
            table[i].is_map = true;
            getsym();
        }
        else {
            error(1);
        }

        /* 处理初始化赋值 */
        if (sym == becomes) {
            getsym();
            memcpy(nxtlev, fsys, sizeof(bool) * symnum);
            nxtlev[semicolon] = true;
            expr_type = parse_expr(lev, ptx, nxtlev);	/* 处理赋值符号右侧表达式 */

            if (table[i].is_str) {
                /* 为一个字符串变量赋值 */
                if (expr_type != type_str) {
                    error(31);
                }
                gen(cpystr, 0, 0);
            } 
            else if (table[i].is_ptr) {
                /* 为一个指针变量赋值 */
                if (expr_type != type_ptr && expr_type != type_int) {
                    error(31);
                }
            }
            else if (table[i].is_set || table[i].is_map) {
                /* 为一个set或map变量赋值 */
                error(44);
            }
            else {
                /* 为一个整型变量赋值 */
                if (expr_type != type_int) {
                    error(31);
                }
            }

            gen(sto, lev - table[i].level, table[i].adr);
        }
        else {
            /* 未初始化也应将变量初始化 */
            if (table[i].is_str) {
                gen(litstr, 0, stacksize + heapsize);
                gen(cpystr, 0, 0);
                gen(sto, lev - table[i].level, table[i].adr);
            }
            else if (table[i].is_map) {
                gen(map_init, 0, 0);
                gen(sto, lev - table[i].level, table[i].adr);
            }
            else if (table[i].is_set) {
                gen(set_init, 0, 0);
                gen(sto, lev - table[i].level, table[i].adr);
            }
            else {
                gen(lit, 0, 0);
                gen(sto, lev - table[i].level, table[i].adr);
            }
        }

        if (sym != semicolon) {
            error(2);
        }
        else {
            getsym();
        }
    }
    /* 分配空间语句 */
    else if (sym == allocsym) {
        getsym();
        if (sym != lparen) {
            error(7);
        }
        getsym();
        if (sym != ident) {
            error(40);
        }
        else {
            i = position(id, *ptx);
            if (i == 0 || !table[i].is_ptr) {
                error(40);
            }
            getsym();
        }
        if (sym != comma) {
            error(41);
        }
        else {
            getsym();
        }
        memcpy(nxtlev, fsys, sizeof(bool) * symnum);
        nxtlev[rparen] = true;	/* 后继符号为) */
        parse_expr(lev, ptx, nxtlev); /* 处理分配空间大小 */
        if (sym == rparen) {
            getsym();
        }
        else {
            error(7);
        }
        /* 生成分配空间指令 */
        gen(alloc, lev - table[i].level, table[i].adr);

        if (sym != semicolon) {
            error(2);
        }
        else {
            getsym();
        }
    }
    /* 释放空间语句 */
    else if (sym == freesym) {
        getsym();
        if (sym != lparen) {
            error(7);
        }
        getsym();
        if (sym != ident) {
            error(42);
        }
        else {
            i = position(id, *ptx);
            if (i == 0 || !table[i].is_ptr) {
                error(42);
            }
            getsym();
        }
        if (sym == rparen) {
            getsym();
        }
        else {
            error(7);
        }
        gen(fre, lev - table[i].level, table[i].adr);
        if (sym != semicolon) {
            error(2);
        }
        else {
            getsym();
        }
    }
	else {
		error(6);
        getsym();
	}
}

/* 
 * 条件处理
 * <bool expr> = <expr> ( "==" | "!=" | "<" | "<=" | ">" | ">=") <expr>
 *
 * lev:     当前分程序所在层
 * ptx:     符号表当前尾指针
 * fsys:    当前模块后继符号集合
*/
void parse_bool_expr(int lev, int* ptx, bool* fsys) {
    enum symbol relop;
    bool nxtlev[symnum];

    /* 逻辑表达式处理 */
    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
    nxtlev[eql] = true;
    nxtlev[neq] = true;
    nxtlev[lss] = true;
    nxtlev[leq] = true;
    nxtlev[gtr] = true;
    nxtlev[geq] = true;
    parse_expr(lev, ptx, nxtlev);
    if (sym != eql && sym != neq && sym != lss && sym != leq && sym != gtr && sym != geq) {
        error(43);
    }
    else {
        relop = sym;
        getsym();
        parse_expr(lev, ptx, fsys);
        switch (relop) {
            case eql:
                gen(opr, 0, 6);
                break;
            case neq:
                gen(opr, 0, 7);
                break;
            case lss:
                gen(opr, 0, 8);
                break;
            case geq:
                gen(opr, 0, 9);
                break;
            case gtr:
                gen(opr, 0, 10);
                break;
            case leq:
                gen(opr, 0, 11);
                break;
            default:
                break;
        }
    }
}

/* 
 * 表达式处理，返回表达式类型
 * <expr> = ["+" | "-"] <term> { ("+" | "-") <term> }
 * 
 * lev:     当前分程序所在层
 * ptx:     符号表当前尾指针
 * fsys:    当前模块后继符号集合
 */
enum ValType parse_expr(int lev, int* ptx, bool* fsys) {
    enum symbol addop;	/* 用于保存正负号 */
    bool nxtlev[symnum];
    enum ValType left_type, right_type; /* 用于类型检查 */

    if (sym == plus || sym == minus) {	/* 表达式开头有正负号，此时当前表达式被看作一个正的或负的项 */
        addop = sym;	/* 保存开头的正负号 */
        getsym();
        memcpy(nxtlev, fsys, sizeof(bool) * symnum);
        nxtlev[plus] = true;
        nxtlev[minus] = true;
        left_type = parse_term(lev, ptx, nxtlev);	/* 处理项 */
        if (left_type != type_int) { /* 正负号只能应用于整数 */
            error(25);
        }
        if (addop == minus) {
            gen(opr, 0, 1);	/* 如果开头为负号生成取负指令 */
        }
    }
    else {	/* 此时表达式被看作项的加减 */
        memcpy(nxtlev, fsys, sizeof(bool) * symnum);
        nxtlev[plus] = true;
        nxtlev[minus] = true;
        left_type = parse_term(lev, ptx, nxtlev);	/* 处理项 */
    }
    while (sym == plus || sym == minus) {
        addop = sym;
        getsym();
        memcpy(nxtlev, fsys, sizeof(bool) * symnum);
        nxtlev[plus] = true;
        nxtlev[minus] = true;
        right_type = parse_term(lev, ptx, nxtlev);

        if (addop == plus) {
            if ((left_type == type_int || left_type == type_ptr) && (right_type == type_int || right_type == type_ptr)) {
                gen(opr, 0, 2);	/* 生成加法指令 */
                left_type = type_int;
            }
            else if (left_type == type_str && right_type == type_str) {
                gen(concat, 0, 0); /* 字符串连接 */
                left_type = type_str;
            }
            else if (left_type == type_str && right_type == type_int) {
                gen(opr, 0, 18); /* 将栈顶整数转换为字符串，再进行连接 */
                gen(concat, 0, 0);
                left_type = type_str;
            }
            else if (left_type == type_int && right_type == type_str) {
                gen(opr, 0, 19); /* 需要先交换栈顶两个元素，让字符串在栈顶，整数在次栈顶 */
                gen(opr, 0, 18); /* 之后才能正常将栈顶整数转换为字符串 */
                gen(concat, 0, 0);
                left_type = type_str;
            }
            else {
                error(26);
            }
        } 
        else {
            if ((left_type == type_int || left_type == type_ptr) && (right_type == type_int || left_type == type_ptr)) {
                gen(opr, 0, 3);	/* 生成减法指令 */
                left_type = type_int;
            } 
            else {
                error(27);
            }
        }
    }

    return left_type;
}

/* 
 * 项处理，返回项类型
 * <term> = <factor> { ("*" | "/") <factor> }
 *  
 * lev:     当前分程序所在层
 * ptx:     符号表当前尾指针
 * fsys:    当前模块后继符号集合
 */
enum ValType parse_term(int lev, int* ptx, bool* fsys) {
    enum symbol mulop;	/* 用于保存乘除法符号 */
    bool nxtlev[symnum];
    enum ValType left_type, right_type; /* 用于类型检查 */

    memcpy(nxtlev, fsys, sizeof(bool) * symnum);
    nxtlev[times] = true;
    nxtlev[slash] = true;
    left_type = parse_factor(lev, ptx, nxtlev);	/* 处理因子 */

    while (sym == times || sym == slash) {
        mulop = sym;
        getsym();
        right_type = parse_factor(lev, ptx, nxtlev);

        if (mulop == times) {
            if (left_type == type_int && right_type == type_int) {
                gen(opr, 0, 4);	/* 生成乘法指令 */
                left_type = type_int;
            } 
            else if (left_type == type_str && right_type == type_int) {
                gen(repeat, 0, 0); /* 字符串重复 */
                left_type = type_str;
            } 
            else if (left_type == type_int && right_type == type_str) {
                gen(opr, 0, 19); /* 需要先交换栈顶两个元素，让字符串在栈顶，整数在次栈顶 */
                gen(repeat, 0, 0); /* 之后再执行字符串重复 */
                left_type = type_str;
            } 
            else {
                error(28);
            }
        } 
        else {
            if (left_type == type_int && right_type == type_int) {
                gen(opr, 0, 5);	/* 生成除法指令 */
                left_type = type_int;
            }
            else {
                error(29);
            }
        }
    }

    return left_type;
}

/* 
 * 因子处理，返回因子类型
 * <factor>= <ident> | <number> | "(" <expr> ")" | <func_call> | <reference> | <dereference> |
 *           <string> | <traversal> | <size> | <look_up> | <find>
 *  
 * lev:     当前分程序所在层
 * ptx:     符号表当前尾指针
 * fsys:    当前模块后继符号集合
 */
enum ValType parse_factor(int lev, int* ptx, bool* fsys) {
    int i;
    bool nxtlev[symnum];
    enum ValType factor_type;

    test(facbegsys, fsys, 11);	/* 检测因子的开始符号 */
    if (inset(sym, facbegsys)) { 	/* 处理因子 */
        /* 因子为变量、引用或函数调用 */
        if (sym == ident) {
            i = position(id, *ptx);	/* 查找标识符在符号表中的位置 */
            if (i == 0) {
                error(3);
            }
            else {
                switch (table[i].kind) {
                    case variable:	/* 为变量或引用 */
                        getsym();
                        /* 字典或集合变量操作 */
                        if (sym == period) {
                            if (!table[i].is_set && !table[i].is_map) {
                                error(45);
                            }
                            getsym();
                            if (sym == findsym) { /* 查找操作 */
                                getsym();
                                if (sym != lparen) {
                                    error(7);
                                }
                                getsym();
                                if (table[i].is_map) {
                                    gen(lit, 0, 0);
                                }
                                enum ValType arg_type = parse_expr(lev, ptx, nxtlev);
                                if (arg_type != type_int) {
                                    error(46);
                                }
                                if (sym != rparen) {
                                    error(8);
                                }
                                getsym();
                                gen(find, lev - table[i].level, table[i].adr);
                                factor_type = type_int;
                            }
                            else if (sym == sizesym) { /* 返回大小操作 */
                                getsym();
                                if (sym != lparen) {
                                    error(7);
                                }    
                                getsym();
                                if (sym != rparen) {
                                    error(8);
                                }
                                getsym();
                                gen(size, lev - table[i].level, table[i].adr);
                                factor_type = type_int;
                            }
                            else if (sym == traversesym) { /* 遍历操作 */
                                getsym();
                                if (sym != lparen) {
                                    error(7);
                                }
                                getsym();
                                if (sym != rparen) {
                                    error(8);
                                }
                                getsym();
                                gen(traverse, lev - table[i].level, table[i].adr);
                                factor_type = type_ptr;
                            } 
                            else if (sym == insertsym || sym == deletesym) { /* 更新操作 */
                                error(49);
                            }
                            else {
                                error(48);
                            }
                        }
                        /* 标识符为指针或字符串，后跟[，是引用；
                           标识符为map，后跟[，是查找 */
                        else if (sym == lbracket) {
                            if (table[i].is_ptr || table[i].is_str) {
                                gen(lod, lev - table[i].level, table[i].adr); /* 读取指针的值（地址）到栈顶 */
                                getsym(); /* 跳过 '[' */
                                memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                                nxtlev[rbracket] = true;
                                enum ValType index_type = parse_expr(lev, ptx, nxtlev); /* 处理索引表达式 */
                                if (index_type != type_int) {
                                    error(30);
                                }
                                if (sym != rbracket) {
                                    error(39);
                                }
                                gen(opr, 0, 17); /* 栈顶为基址，次栈顶为索引，生成新栈顶为安全计算后的地址 */
                                gen(loda, 0, 0); /* 解引用得到字符值 */
                                getsym(); /* 跳过 ']' */
                                factor_type = type_int;
                            }
                            else if (table[i].is_map) {
                                getsym(); /* 跳过 '[' */
                                memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                                nxtlev[rbracket] = true;
                                gen(lit, 0, 1);
                                enum ValType index_type = parse_expr(lev, ptx, nxtlev); /* 处理索引表达式 */
                                if (index_type != type_int) {
                                    error(30);
                                }
                                if (sym != rbracket) {
                                    error(39);
                                }
                                gen(find, lev - table[i].level, table[i].adr);
                                getsym(); /* 跳过 ']' */
                                factor_type = type_int;
                            }
                            else {
                                error(36);
                            }
                        }
                        /* 普通变量 */
                        else {
                            if (table[i].is_str) {
                                /* 字符串变量，加载其地址并增加引用计数 */
                                gen(lod, lev - table[i].level, table[i].adr);
                                gen(refinc, 0, 0);
                                factor_type = type_str;
                            } 
                            else if (table[i].is_ptr) {
                                /* 指针变量，加载其地址 */
                                gen(lod, lev - table[i].level, table[i].adr);
                                factor_type = type_ptr;
                            }
                            else {
                                /* 整型变量，加载其值 */
                                gen(lod, lev - table[i].level, table[i].adr);
                                factor_type = type_int;
                            }
                        }
                        break;
                    /* 为过程调用 */
                    case function:
                        getsym();
                        if (sym != lparen) {
                            error(7);
                        }

                        if (table[i].num_params != 0) {
                            memcpy(nxtlev, fsys, sizeof(bool) * symnum);
                            nxtlev[comma] = true;
                            nxtlev[rparen] = true;
                            getsym();
                            /* 参数在符号表中连续存储，顺序读取 */
                            int param_cnt = 0;
                            enum ValType param_type;
                            param_type = parse_expr(lev, ptx, nxtlev);
                            if (table[i+1].is_str && param_type == type_str) {
                                gen(refinc, 0, 0);
                            }
                            param_cnt++;
                            while (sym == comma) {
                                getsym();
                                param_cnt++;
                                param_type = parse_expr(lev, ptx, nxtlev);
                                if (table[i+1].is_str && param_type == type_str) {
                                    gen(refinc, 0, 0);
                                }
                            }

                            if (param_cnt != table[i].num_params) {
                                error(22);
                            }
                        }
                        else {
                            getsym();
                        }

                        if (sym != rparen) {
                            error(8);
                        }
                        gen(cal, lev - table[i].level, table[i].adr);	/* 生成call指令 */
                        getsym();
                        factor_type = table[i].return_type;
                        break;
                    default:
                        error(10);
                        break;
                }
            }
        }
        /* 因子为数 */
        else if (sym == number) {
            if (num > amax) {
                error(13);
                num = 0;
            }
            gen(lit, 0, num);
            getsym();
            factor_type = type_int;
        }
        /* 因子为表达式 */
        else if (sym == lparen)  {
            getsym();
            memcpy(nxtlev, fsys, sizeof(bool) * symnum);
            nxtlev[rparen] = true;
            factor_type = parse_expr(lev, ptx, nxtlev);
            if (sym == rparen) {
                getsym();
            }
            else {
                error(8);
            }
        }
        /* 因子为解引用 */
        else if (sym == times) {
            getsym();
            if (sym != ident) {
                error(36);
            }
            
            i = position(id, *ptx);
            if (i == 0) {
                error(3);
            }
            else if (table[i].kind != variable) {
                error(36);
            }
            gen(lod, lev - table[i].level, table[i].adr);
            gen(loda, 0, 0);
            getsym();
            factor_type = type_int;
        }
        /* 因子为引用 */
        else if (sym == amper) {
            getsym();
            if (sym == ident) {
                i = position(id, *ptx);
                if (i == 0) {
                    error(3);
                } 
                else if (table[i].kind != variable) {
                    error(37);
                }
                else {
                    gen(addr, lev - table[i].level, table[i].adr);
                }
                getsym();
                factor_type = type_ptr;
            }
            else { /* 赋值语句左部标识符类型错误*/
                error(4);
            }
        }
        /* 因子为字符串字面量 */
        else if (sym == string) {
            gen(litstr, 0, num); /* num是字面量在静态区内的地址 */
            getsym();
            factor_type = type_str;
        }
        test(fsys, facbegsys, 12); /* 一个因子处理完毕，遇到的单词应在fsys集合中 */
        /* 如果不是，报错并找到下一个因子的开始，使语法分析可以继续运行下去 */
    }
    else { /* 因子后续符号错误 */
        error(12);
    }

    return factor_type;
}

/*
 * 在符号表中加入一项
 *
 * k:      标识符的种类为program，var或func
 * ptx:    符号表尾指针的指针，为了可以改变符号表尾指针的值
 * lev:    标识符所在的层次
 * pdx:    dx为当前应分配的变量的相对地址，分配后要增加1
 */
void enter(enum object k, int* ptx, int lev, int* pdx) {
    (*ptx)++;
    strcpy(table[(*ptx)].name, id); /* 符号表的name域记录标识符的名字 */
    table[(*ptx)].kind = k;
    table[(*ptx)].is_ptr = false;
    table[(*ptx)].is_str = false;
    table[(*ptx)].is_map = false;
    table[(*ptx)].is_set = false;
    switch (k) {
        case program:   /* 程序 */
            table[(*ptx)].level = lev;
            break;
        case variable:	/* 变量 */
            table[(*ptx)].level = lev;
            table[(*ptx)].adr = (*pdx);
            (*pdx)++;
            break;
        case function:	/* 过程 */
            table[(*ptx)].level = lev;
            break;
    }
}

/*
 * 查找标识符在符号表中的位置，从tx开始倒序查找标识符
 * 找到则返回在符号表中的位置，否则返回0
 *
 * id:    要查找的名字
 * tx:    当前符号表尾指针
 */
int position(char* id, int tx) {
    int i;
    strcpy(table[0].name, id);
    i = tx;
    while (strcmp(table[i].name, id) != 0) {
        i--;
    }
    return i;
}

/* 输出符号表 */
void listtable(int *ptx) {
    if (tableswitch) {
        for (int i = 1; i <= *ptx; i++) {
            switch (table[i].kind) {
            case program:
                printf("    %d program %s ", i, table[i].name);
                printf("lev=%d addr=%d size=%d\n", table[i].level, table[i].adr, table[i].size);
                fprintf(ftable, "    %d program  %s ", i, table[i].name);
                fprintf(ftable, "lev=%d addr=%d size=%d\n", table[i].level, table[i].adr, table[i].size);
                break;
            case variable:
                printf("    %d var   %s ", i, table[i].name);
                printf("lev=%d addr=%d ", table[i].level, table[i].adr);
                if (table[i].is_ptr) printf("ptr");
                if (table[i].is_str) printf("str");
                if (table[i].is_set) printf("set");
                if (table[i].is_map) printf("map");
                printf("\n");

                fprintf(ftable, "    %d var   %s ", i, table[i].name);
                fprintf(ftable, "lev=%d addr=%d ", table[i].level, table[i].adr);
                if (table[i].is_ptr) fprintf(ftable, "ptr");
                if (table[i].is_str) fprintf(ftable, "str");
                if (table[i].is_set) fprintf(ftable, "set");
                if (table[i].is_map) fprintf(ftable, "map");
                fprintf(ftable, "\n");
                break;
            case function:
                printf("    %d func  %s ", i, table[i].name);
                printf("lev=%d addr=%d size=%d num_params=%d\n", table[i].level, table[i].adr, table[i].size, table[i].num_params);
                fprintf(ftable, "    %d func  %s ", i, table[i].name);
                fprintf(ftable, "lev=%d addr=%d size=%d num_params=%d\n", table[i].level, table[i].adr, table[i].size, table[i].num_params);
                break;
            }
        }
        printf("\n");
        fprintf(ftable, "\n");
    }
}

/*
 * 生成虚拟机代码
 *
 * x: instruction.f;
 * y: instruction.l;
 * z: instruction.a;
 */
void gen(enum fct x, int y, int z) {
    if (cx >= cxmax) {
        printf("Program is too long!\n");
        fprintf(foutput, "Program is too long!\n");
        exit(1);
    }
    if (z >= amax) {
        printf("Displacement address is too big!\n");	/* 地址偏移越界 */
        fprintf(foutput, "Displacement address is too big!\n");
        exit(1);
    }
    code[cx].f = x;
    code[cx].l = y;
    code[cx].a = z;
    // printf("gen %d: %s %d %d\n", cx, mnemonic[code[cx].f], code[cx].l, code[cx].a);
    cx++;
}

/* 
 * 输出目标代码清单
 *
 * cx0:    代码起始位置
 */
void listcode(int cx0) {
    int i;
    if (listswitch) {
        printf("\n");
        for (i = cx0; i < cx; i++) {
            printf("%d %s %d %d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
        }
    }
}

/* 输出所有目标代码 */
void listall() {
    int i;
    if (listswitch) {
        for (i = 0; i < cx; i++) {
            printf("%d %s %d %d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
            fprintf(fcode, "%d %s %d %d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
        }
    }
}

/* 
 * 在堆区尝试分配一个大小为size的连续空闲块，
 * 若成功，返回起始索引，未找到返回0
 * 
 * size: 需要分配的实际大小
 * avail: 可用性数组
 */
int try_alloc(int size, int* avail) {
    if (size <= 0) {
        return 0;
    }
    /* 遍历堆区 */
    int count = 0;
    int candidate = 0;
    for (int k = stacksize; k < stacksize + heapsize; k++) {
        /* 找到空闲块 */
        if (!avail[k]) {
            if (count == 0) {
                candidate = k;
            }
            count++;
            /* 连续空闲块足够大 */
            if (count >= size) {
                /* 分配块 */
                for (int i = 0; i < size; i++) {
                    avail[candidate + i] = 1;
                }
                return candidate;
            }
        } 
        /* 连续块断开，重新计数 */
        else {
            count = 0;
            candidate = 0;
        }
    }
    return 0;
}

/* 
 * 释放堆区的一个块
 * 
 * start_ptr: 指向存有分配空间大小的起始索引
 * avail: 可用性数组
 */
void free_heap_block(int start_ptr, int* s, int* avail) {
    if (start_ptr < stacksize || start_ptr >= stacksize + heapsize) {
        /* 不能手动释放的区域 */
        return;
    }
    if (!avail[start_ptr]) {
        /* 已经释放的区域 */
        return;
    }
    
    int block_size = s[start_ptr];
    if (start_ptr + block_size > stacksize + heapsize) {
        /* 大小异常 */
        printf("Error: Block size exceed\n");
        fprintf(fresult, "Error: Block size exceed\n");
        exit(1);
    }

    /* 释放空间 */
    memset(s + start_ptr, 0, sizeof(int) * block_size);
}

/* 
 * 字符串转换为整数数组
 * 
 * c_str: 原字符串
 * int_arr: 目标整数数组
 */
void str2int(char* c_str, int* int_arr) {
    int len = strlen(c_str);
    for (int i = 0; i < len; i++) {
        int_arr[i] = (int)c_str[i];
    }
    
    int_arr[len] = 0;
}

/* 
 * 整数数组转换为字符串
 * 
 * int_arr: 原整数数组
 * c_str: 目标字符串
 */
void int2str(int* int_arr, char* c_str) {
    int i = 0;
    while (int_arr[i] != 0) {
        c_str[i] = (char)int_arr[i];
        i++;
    }
    
    c_str[i] = '\0';
}

/* 
 * 获取整数数组表示的字符串长度
 * 
 * int_arr: 整数数组
 */
int get_str_len(int* int_arr) {
    int len = 0;
    while (int_arr[len] != 0) {
        len++;
    }
    return len;
}

/* 
 * 分配一个新非根集合节点在堆中
 * 
 * s: 内存区
 * avail: 可用性数组
 * l_ptr: 新节点左指针
 * val: 节点的值
 * r_ptr: 新节点右指针
 */
int alloc_set_node(int* s, int* avail, int l_ptr, int val, int r_ptr) {
    int start = try_alloc(3, avail);
    if (start == 0) {
        printf("Heap overflow node alloc\n");
        fprintf(fresult, "Heap overflow for node alloc\n");
        exit(1);
    }
    s[start] = l_ptr;
    s[start + 1] = val;
    s[start + 2] = r_ptr;
    return start;
}

/* 
 * 分配一个新非根字典节点在堆中
 * 
 * s: 内存区
 * avail: 可用性数组
 * l_ptr: 新节点左指针
 * key: 节点的关键字
 * val: 节点的值
 * r_ptr: 新节点右指针
 */
int alloc_map_node(int* s, int* avail, int l_ptr, int key, int val, int r_ptr) {
    int start = try_alloc(4, avail);
    if (start == 0) {
        printf("Heap overflow node alloc\n");
        fprintf(fresult, "Heap overflow for node alloc\n");
        exit(1);
    }
    s[start] = l_ptr;
    s[start + 1] = key;
    s[start + 2] = val;
    s[start + 3] = r_ptr;
    return start;
}

/* 
 * 递归释放集合节点及其子节点
 * 
 * node_ptr: 根节点的地址
 * s: 内存区
 * avail: 可用性数组
 */
void free_set_node(int node_ptr, int* s, int* avail) {
    if (node_ptr == 0) return;

    free_set_node(s[node_ptr], s, avail);
    free_set_node(s[node_ptr + 2], s, avail);

    for (int k = 0; k < 3; k++) {
        avail[node_ptr + k] = 0;
    }
}

/* 
 * 递归释放字典节点及其子节点
 *
 * node_ptr: 根节点的地址
 * s: 内存区
 * avail: 可用性数组
 */
void free_map_node(int node_ptr, int* s, int* avail) {
    if (node_ptr == 0) return;

    free_map_node(s[node_ptr], s, avail);
    free_map_node(s[node_ptr + 3], s, avail);

    for (int k = 0; k < 4; k++) {
        avail[node_ptr + k] = 0;
    }
}

/* 
 * 中序遍历集合树并收集值到数组
 *
 * node_ptr: 根节点的地址
 * s: 内存区
 * index: 数组当前待插入的地址
 */
void collect_set_values(int node_ptr, int* s, int* index) {
    if (node_ptr == 0) return;
    collect_set_values(s[node_ptr], s, index);
    s[*index] = s[node_ptr + 1];
    (*index)++;
    collect_set_values(s[node_ptr + 2], s, index);
}

/* 
 * 中序遍历字典树并收集值到数组
 *
 * node_ptr: 根节点的地址
 * s: 内存区
 * index: 数组当前待插入的地址
 */
void collect_map_values(int node_ptr, int* s, int* index) {
    if (node_ptr == 0) return;
    collect_map_values(s[node_ptr], s, index);
    s[*index] = s[node_ptr + 1];
    (*index)++;
    s[*index] = s[node_ptr + 2];
    (*index)++;
    collect_map_values(s[node_ptr + 3], s, index);
}

/* 解释程序 */
void interpret() {
    int p = 0; /* 指令指针 */
    int b = 1; /* 指令基址 */
    int t = 0; /* 栈顶指针 */
    struct instruction i;	/* 存放当前指令 */
    int s[stacksize + heapsize + staticsize];  /* 栈区 + 堆区 + 静态存储区 */
    int avail[stacksize + heapsize];           /* 可用性数组（实际只有heapsize段可用，扩展是为了统一下标） */

    /* 将字符串字面量保存到静态存储区 */
    int static_offset = 0;
    for (int i = 0; i < const_str_num; i++) {
        char* c_str = const_str[i];
        int len = strlen(c_str);
        int alloc_size = len + 1;

        for (int k = 0; k < len; k++) {
            s[stacksize + heapsize + static_offset + k] = (int)c_str[k];
        }
        s[stacksize + heapsize + static_offset + len] = 0;
        static_offset += alloc_size;
        s[stacksize + heapsize + static_offset] = -1;
    }

    // for(int i = stacksize + heapsize; i < stacksize + heapsize + static_offset; i++) {
    //     printf("%d, " ,s[i]);
    // }
    // printf("\n");

    printf("Start pl0\n");
    fprintf(fresult, "Start pl0\n");
    s[0] = 0; /* s[0]不用 */
    s[1] = 0; /* 主程序的三个联系单元均置为0 */
    s[2] = 0;
    s[3] = 0;

    memset(avail, 0, sizeof(int) * (stacksize + heapsize));
    memset(avail, 1, sizeof(int) * stacksize);

    do {
        if (p < 0 || p >= cxmax) {
            printf("Error: Invalid program counter %d\n", p);
            fprintf(fresult, "Error: Invalid program counter %d\n", p);
            p = 0;
            break;
        }

        i = code[p];	/* 读当前指令 */
        p = p + 1;

        switch (i.f) {
            case lit:	/* 将常量a的值取到栈顶 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                t = t + 1;
                s[t] = i.a;
                break;
            case opr:	/* 数学、逻辑运算 */
                switch (i.a) {
                    case 0: {/* 函数调用结束后返回 */
                        int temp = s[t]; /* 保存返回值 */
                        t = b - 1;
                        p = s[t + 3];
                        b = s[t + 2];
                        t = t - i.l;
                        t = t + 1;
                        s[t] = temp; /* 压入返回值 */
                        }
                        break;
                    case 1: /* 栈顶元素取反 */
                        s[t] = -s[t];
                        break;
                    case 2: /* 次栈顶项加上栈顶项，退两个栈元素，相加值进栈 */
                        t = t - 1;
                        s[t] = s[t] + s[t + 1];
                        break;
                    case 3:/* 次栈顶项减去栈顶项 */
                        t = t - 1;
                        s[t] = s[t] - s[t + 1];
                        break;
                    case 4:/* 次栈顶项乘以栈顶项 */
                        t = t - 1;
                        s[t] = s[t] * s[t + 1];
                        break;
                    case 5:/* 次栈顶项除以栈顶项 */
                        t = t - 1;
                        s[t] = s[t] / s[t + 1];
                        break;
                    case 6:/* 次栈顶项与栈顶项是否相等 */
                        t = t - 1;
                        s[t] = (s[t] == s[t + 1]);
                        break;
                    case 7:/* 次栈顶项与栈顶项是否不等 */
                        t = t - 1;
                        s[t] = (s[t] != s[t + 1]);
                        break;
                    case 8:/* 次栈顶项是否小于栈顶项 */
                        t = t - 1;
                        s[t] = (s[t] < s[t + 1]);
                        break;
                    case 9:/* 次栈顶项是否大于等于栈顶项 */
                        t = t - 1;
                        s[t] = (s[t] >= s[t + 1]);
                        break;
                    case 10:/* 次栈顶项是否大于栈顶项 */
                        t = t - 1;
                        s[t] = (s[t] > s[t + 1]);
                        break;
                    case 11: /* 次栈顶项是否小于等于栈顶项 */
                        t = t - 1;
                        s[t] = (s[t] <= s[t + 1]);
                        break;
                    case 12:/* 栈顶值输出 */
                        printf("%d", s[t]);
                        fprintf(fresult, "%d", s[t]);
                        t = t - 1;
                        break;
                    case 13: {/* 栈顶字符串输出 */
                        int str_ptr = s[t];
                        char output_str[heapsize];

                        if (str_ptr == 0) {
                            printf("null");
                            fprintf(fresult, "null");
                        }
                        else if (str_ptr >= stacksize && str_ptr < stacksize + heapsize + staticsize) {
                            int2str(s + str_ptr, output_str);
                            printf("%s", output_str);
                            fprintf(fresult, "%s", output_str);
                        }
                        else {
                            printf("Error at line %d: Invalid string pointer\n", p - 1);
                            fprintf(fresult, "Error at line %d: Invalid string pointer\n", p - 1);
                            exit(1);
                        }
                        t = t - 1;
                        }
                        break;
                    case 14:/* 输出换行符 */
                        printf("\n");
                        fprintf(fresult, "\n");
                        break;
                    case 15:/* 读入一个整型输入置于栈顶 */
                        if (t + 1 >= stacksize) {
                            printf("Stack overflow at line %d\n", p - 1);
                            fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                            exit(1);
                        }
                        t = t + 1;
                        printf("? ");
                        fprintf(fresult, "? ");
                        scanf("%d", &(s[t]));
                        fprintf(fresult, "%d\n", s[t]);
                        break;
                    case 16: {/* 读取字符串输入，将其堆地址压入栈顶 */
                        if (t + 1 >= stacksize) {
                            printf("Stack overflow at line %d\n", p - 1);
                            fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                            exit(1);
                        }
                        char input_str[strmax + 1];

                        printf("(str)? ");
                        fprintf(fresult, "(str)? ");
                        
                        scanf("%s", input_str); 
                        fprintf(fresult, "%s\n", input_str);

                        int len = strlen(input_str);
                        int alloc_size = len + 3; 

                        int start = try_alloc(alloc_size, avail);
                        if (start == 0) {
                            printf("Error at line %d: Heap overflow for input\n", p - 1);
                            fprintf(fresult, "Error at line %d: Heap overflow for input\n", p - 1);
                            exit(1);
                        }

                        s[start] = alloc_size; /* 在头部存储总分配大小 */
                        s[start + 1] = 1;      /* 初始化引用计数器 */

                        /* 字符串转换为整数数组 */
                        str2int(input_str, s + start + 2);

                        t = t + 1;
                        s[t] = start + 2;
                        }
                        break;
                    case 17: {/* 安全内存加法 */
                        int index_val = s[t];
                        int base_addr = s[t-1];
                        t = t - 2;

                        /* 内存安全检查 */
                        if (base_addr <= stacksize || base_addr > stacksize + heapsize) {
                            printf("Error at line %d: Invalid base address\n", p - 1);
                            fprintf(fresult, "Error at line %d: Invalid base address\n", p - 1);
                            exit(1);
                        }
                        int alloc_size = s[base_addr - 2];
                        if (index_val < 0 || index_val >= alloc_size) {
                            printf("Error at line %d: Invalid index address\n", p - 1);
                            fprintf(fresult, "Error at line %d: Invalid index address\n", p - 1);
                            exit(1);
                        }

                        /* 安全检查通过 */
                        t = t + 1;
                        s[t] = base_addr + index_val;
                        }
                        break;
                    case 18: {/* 将栈顶整数转换为字符串 */
                        int val = s[t];
                        char temp_str[strmax];
                        sprintf(temp_str, "%d", val);
                        
                        int len = strlen(temp_str);
                        int alloc_size = len + 3;

                        /* 转换后，在堆中分配空间 */
                        int str_base = try_alloc(alloc_size, avail);
                        if (str_base == 0) {
                            printf("Error at line %d: Heap overflow for int2str\n", p - 1);
                            fprintf(fresult, "Error at line %d: Heap overflow for int2str\n", p - 1);
                            exit(1);
                        }

                        s[str_base] = alloc_size;
                        s[str_base + 1] = 1;
                        str2int(temp_str, s + str_base + 2);

                        s[t] = str_base + 2;
                        }
                        break;
                    case 19: {/* 交换栈顶两个元素 */
                        int temp = s[t];
                        s[t] = s[t-1];
                        s[t-1] = temp;
                        }
                        break;
                    case 20:/* 弹出栈顶元素 */
                        t = t - 1;
                        break;
                }
                break;
            case lod:/* 取相对当前过程的数据基地址为a的内存的值到栈顶 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                t = t + 1;
                s[t] = s[base(i.l, s, b) + i.a];
                break;
            case sto:/* 栈顶的值存到相对当前过程的数据基地址为a的内存 */
                s[base(i.l, s, b) + i.a] = s[t];
                t = t - 1;
                break;
            case cal:/* 调用子过程 */
                if (t + 3 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                s[t + 1] = base(i.l, s, b);	/* 将父过程基地址入栈，即建立静态链 */
                s[t + 2] = b;	/* 将本过程基地址入栈，即建立动态链 */
                s[t + 3] = p;	/* 将当前指令指针入栈，即保存返回地址 */
                b = t + 1;	/* 改变基地址指针值为新过程的基地址 */
                p = i.a;	/* 跳转 */
                break;
            case ini:/* 在数据栈中为函数开辟a个单元的数据区 */
                if (t + i.a >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                t = t + i.a;
                break;
            case jmp:/* 直接跳转 */
                p = i.a;
                break;
            case jpc:/* 条件跳转 */
                if (s[t] == 0) {
                    p = i.a;
                }
                t = t - 1;
                break;
            case addr:/* 取地址到栈顶 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                t = t + 1;
                s[t] = base(i.l, s, b) + i.a;
                break;
            case loda: {/* 解栈顶的地址到栈顶 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                int addr = s[t];
                if (addr > stacksize + heapsize || addr < 0) {
                    printf("Error at line %d: Invalid address\n", p - 1);
                    fprintf(fresult, "Error at line %d: Invalid address\n", p - 1);
                    exit(1);
                }
                s[t] = s[addr];
                }
                break;
            case stoa:/* 栈顶的值存到栈顶-1的地址 */
                s[s[t-1]] = s[t];
                t = t - 2;
                break;
            case alloc: {/* 为指针分配栈顶值大小的空间 */
                if (s[t] <= 0) {
                    printf("Error at line %d: Alloc size < 0\n", p - 1);
                    fprintf(fresult, "Error at line %d: Alloc size < 0\n", p - 1);
                    exit(1);
                }
                int num_ele = s[t];
                t = t - 1;  /* 将待分配的大小出栈 */
                int alloc_size = num_ele + 2;
                int start = try_alloc(alloc_size, avail);
                /* 分配空间失败，退出 */
                if (!start) {
                    printf("Error at line %d: Heap overflow for alloc\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for alloc\n", p - 1);
                    exit(1);
                }
                /* 保存可用空间大小 */
                s[start] = alloc_size;
                s[start + 1] = 1;
                start += 2;
                s[base(i.l, s, b) + i.a] = start; /* 先将栈顶地址存到待分配的指针内 */
                }
                break;
            case fre: {/* 回收指针分配的空间 */
                int start = s[base(i.l, s, b) + i.a];
                start -= 2;
                free_heap_block(start, s, avail);
                }
                break;
            case cpystr: {/* 将栈顶字符串复制到堆中，压入新分配的地址 */
                int ori_str = s[t];
                t = t - 1;

                /* 处理空指针 */
                if (ori_str == 0) {
                    t = t + 1;
                    s[t] = 0;
                    break;
                }

                char temp_str[heapsize];
                int str_len = 0;

                /* 从静态区复制字符串字面量 */
                if (ori_str >= stacksize + heapsize && ori_str < stacksize + heapsize + staticsize) {
                    int char_idx = 0;
                    while (s[ori_str + char_idx] != 0 && char_idx < heapsize - 1) {
                        temp_str[char_idx] = (char)s[ori_str + char_idx];
                        char_idx++;
                    }
                    temp_str[char_idx] = '\0';
                    str_len = char_idx;
                }
                /* 从堆区复制字符串 */
                else if (ori_str >= stacksize && ori_str < stacksize + heapsize) {
                    int2str(s + ori_str, temp_str);
                    str_len = get_str_len(s + ori_str);
                }
                else {
                    printf("Error at line %d: Invalid address for cpystr\n", p - 1);
                    fprintf(fresult, "Error at line %d: Invalid address for cpystr\n", p - 1);
                    exit(1);
                }

                /* 为目标字符串分配空间 */
                int alloc_size = str_len + 3;
                int new_str = try_alloc(alloc_size, avail);

                if (new_str == 0) {
                    printf("Error at line %d: Heap overflow for cpystr\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for cpystr\n", p - 1);
                    exit(1);
                }

                s[new_str] = alloc_size;
                s[new_str + 1] = 1;

                str2int(temp_str, s + new_str + 2);

                t = t + 1;
                s[t] = new_str + 2;
                }
                break;
            case litstr: {/* 将字符串的值取到栈顶 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }

                int static_str = i.a;

                if (static_str < stacksize + heapsize || static_str >= stacksize + heapsize + staticsize) {
                    printf("Error at line %d: Invalid static address\n", p - 1);
                    fprintf(fresult, "Error at line %d: Invalid static address\n", p - 1);
                    exit(1);
                }

                t = t + 1;
                s[t] = static_str;
                }
                break;
            case refinc: {/* 栈顶字符串引用计数递增 */
                int str_ptr = s[t];
                /* 不处理空指针 */
                if (str_ptr == 0) {
                    break;
                }
                /* 不处理静态字符串 */
                if (str_ptr >= stacksize + heapsize && str_ptr < stacksize + heapsize + staticsize) {
                    break;
                }
                if (str_ptr < stacksize || str_ptr >= stacksize + heapsize + staticsize) {
                    printf("Error at line %d: Invalid address for refinc\n", p - 1);
                    fprintf(fresult, "Error at line %d: Invalid address for refinc\n", p - 1);
                    exit(1);
                }
                s[str_ptr - 1]++;
                }
                break;
            case refdec: {/* 栈顶字符串引用计数递减、自动释放 */
                int str_ptr = s[t];
                /* 不处理空指针 */
                if (str_ptr == 0) {
                    break;
                }
                /* 不处理静态字符串 */
                if (str_ptr >= stacksize + heapsize && str_ptr < stacksize + heapsize + staticsize) {
                    break;
                }
                if (str_ptr < stacksize || str_ptr >= stacksize + heapsize + staticsize) {
                    printf("Error at line %d: Invalid address for refdec\n", p - 1);
                    fprintf(fresult, "Error at line %d: Invalid address for refdec\n", p - 1);
                    exit(1);
                }
                s[str_ptr - 1]--;

                /* 引用计数器为0， 释放 */
                if (s[str_ptr - 1] == 0) {
                    free_heap_block(str_ptr - 2, s, avail);
                }
                }
                break;
            case concat: {/* 栈顶两字符串连接，结果入栈 */
                int str2_ptr = s[t];
                int str1_ptr = s[t - 1];
                t = t - 2;

                char str1[heapsize] = "";
                char str2[heapsize] = "";

                if (str1_ptr != 0) {
                    /* 处理静态区字符串 */
                    if (str1_ptr >= stacksize + heapsize && str1_ptr < stacksize + heapsize + staticsize) {
                        int j = 0; 
                        while (s[str1_ptr + j] != 0 && j < heapsize) { 
                            str1[j] = (char)s[str1_ptr + j]; 
                            j++; 
                        }
                        str1[j] = '\0';
                    }
                    /* 处理堆区字符串 */
                    else if (str1_ptr >= stacksize && str1_ptr < stacksize + heapsize) {
                        int2str(s + str1_ptr, str1);
                    } 
                    else {
                        printf("Error at line %d: Invalid address in left str\n", p - 1);
                        fprintf(fresult, "Error at line %d: Invalid address in left str\n", p - 1);
                        exit(1);
                    }
                }
                if (str2_ptr != 0) {
                    /* 处理静态区字符串 */
                    if (str2_ptr >= stacksize + heapsize && str2_ptr < stacksize + heapsize + staticsize) {
                        int i_char = 0; 
                        while (s[str2_ptr + i_char] != 0 && i_char < heapsize) { 
                            str2[i_char] = (char)s[str2_ptr + i_char]; 
                            i_char++; 
                        } 
                        str2[i_char] = '\0';
                    }
                    /* 处理堆区字符串 */
                    else if (str2_ptr >= stacksize && str2_ptr < stacksize + heapsize) {
                        int2str(s + str2_ptr, str2);
                    } 
                    else {
                        printf("Error at line %d: Invalid address in right str\n", p - 1);
                        fprintf(fresult, "Error at line %d: Invalid address in right str\n", p - 1);
                        exit(1);
                    }
                }

                int len1 = strlen(str1);
                int len2 = strlen(str2);
                int total_len = len1 + len2;

                if (total_len >= heapsize) {
                    printf("Error at line %d: Heap overflow for concat\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for concat\n", p - 1);
                    exit(1);
                }

                /* 字符串连接 */
                char result[heapsize];
                strcpy(result, str1);
                strcat(result, str2);

                int len = strlen(result);
                if (len >= heapsize) {
                    printf("Error at line %d: Heap overflow for concat\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for concat\n", p - 1);
                    exit(1);
                }

                /* 为结果分配空间 */
                int alloc_size = len + 3;
                int result_ptr = try_alloc(alloc_size, avail);
                if (result_ptr == 0) {
                    printf("Error at line %d: Heap overflow for concat\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for concat\n", p - 1);
                    exit(1);
                }
                s[result_ptr] = alloc_size;
                s[result_ptr + 1] = 1;
                str2int(result, s + result_ptr + 2);

                /* 结果入栈 */
                t = t + 1;
                s[t] = result_ptr + 2;

                /* 递减引用 */
                if (str1_ptr != 0 && str1_ptr >= stacksize && str1_ptr < stacksize + heapsize) {
                    s[str1_ptr - 1]--;
                    if (s[str1_ptr - 1] == 0) {
                        free_heap_block(str1_ptr - 2, s, avail);
                    }
                }
                if (str2_ptr != 0 && str2_ptr >= stacksize && str2_ptr < stacksize + heapsize) {
                    s[str2_ptr - 1]--;
                    if (s[str2_ptr - 1] == 0) {
                        free_heap_block(str2_ptr - 2, s, avail);
                    }
                }
                }
                break;
            case repeat: { /* 栈顶字符串重复次栈顶整数次，结果入栈 */
                int repeat_cnt = s[t];
                int str_ptr = s[t - 1];
                t = t - 2;

                if (repeat_cnt < 0) {
                    printf("Error at line %d: Repeat count < 0\n", p - 1);
                    fprintf(fresult, "Error at line %d: Repeat count < 0\n", p - 1);
                    exit(1);
                }

                char c_str[heapsize] = "";
                if (str_ptr != 0) {
                    /* 处理静态区字符串 */
                    if (str_ptr >= stacksize + heapsize && str_ptr < stacksize + heapsize + staticsize) {
                        int j = 0; 
                        while (s[str_ptr + j] != 0 && j < heapsize) { 
                            c_str[j] = (char)s[str_ptr + j]; 
                            j++; 
                        } 
                        c_str[j] = '\0';
                    }
                    /* 处理堆区字符串 */
                    else if (str_ptr >= stacksize && str_ptr < stacksize + heapsize) {
                        int2str(s + str_ptr, c_str);
                    }
                    else {
                        printf("Error at line %d: Invalid address in str\n", p - 1);
                        fprintf(fresult, "Error at line %d: Invalid address in str\n", p - 1);
                        exit(1);
                    }
                }

                /* 计算字符串大小 */
                char result[heapsize] = "";
                int len_current = 0;
                int len_ori = strlen(c_str);
                if (len_ori * repeat_cnt >= heapsize) {
                    printf("Error at line %d: Heap overflow for repeat\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for repeat\n", p - 1);
                    exit(1);
                }

                /* 进行字符串复制 */
                for (int k = 0; k < repeat_cnt; k++) {
                    strcat(result, c_str);
                    len_current += len_ori;
                }
                result[len_current] = '\0';

                /* 为结果分配空间 */
                int len = strlen(result);
                int alloc_size = len + 3;
                
                int result_ptr = try_alloc(alloc_size, avail);
                if (result_ptr == 0) {
                    printf("Error at line %d: Heap overflow for repeat\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for repeat\n", p - 1);
                    exit(1);
                }
                s[result_ptr] = alloc_size;
                s[result_ptr + 1] = 1;
                str2int(result, s + result_ptr + 2);

                /* 结果入栈 */
                t = t + 1;
                s[t] = result_ptr + 2;

                /* 递减引用 */
                if (str_ptr != 0 && str_ptr >= stacksize && str_ptr < stacksize + heapsize) {
                    s[str_ptr - 1]--;
                    if (s[str_ptr - 1] == 0) { 
                        free_heap_block(str_ptr - 2, s, avail);
                    }
                }
                }
                break;
            case map_init: { /* 新建一个字典，将其根节点地址入栈 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                int alloc_size = 6;
                int start = try_alloc(alloc_size, avail);
                /* 分配空间失败，退出 */
                if (!start) {
                    printf("Error at line %d: Heap overflow for set init\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for set init\n", p - 1);
                    exit(1);
                }
                /* 保存可用空间大小 */
                s[start] = 0;
                s[start + 1] = 0; /* type = map */
                s[start + 2] = 0;
                s[start + 3] = 0;
                s[start + 4] = 0;
                s[start + 5] = 0;
                t += 1;
                s[t] = start;
                }
                break;
            case set_init: { /* 新建一个集合，将其根节点地址入栈 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                int alloc_size = 5;
                int start = try_alloc(alloc_size, avail);
                /* 分配空间失败，退出 */
                if (!start) {
                    printf("Error at line %d: Heap overflow for set init\n", p - 1);
                    fprintf(fresult, "Error at line %d: Heap overflow for set init\n", p - 1);
                    exit(1);
                }
                /* 保存可用空间大小 */
                s[start] = 0;
                s[start + 1] = 1; /* type = set */
                s[start + 2] = 0;
                s[start + 3] = 0;
                s[start + 4] = 0;
                t += 1;
                s[t] = start;
                }
                break;
            case tree_free: { /* 释放字典/集合变量 */
                int root = s[t];
                if (s[root - 1] == 0) { /* 字典释放 */
                    free_map_node(root + 2, s, avail);
                }
                else { /* 集合释放 */
                    free_set_node(root + 2, s, avail);
                }
                avail[root] = 0;
                avail[root + 1] = 0;
                }
                break;
            case insert: { /* 将栈顶值（对）插入字典/集合中 */
                int current_root = s[base(i.l, s, b) + i.a];
                int size = s[current_root];
                current_root += 2;
                if (s[current_root - 1] == 0) { /* 字典插入 */
                    int val = s[t];
                    int key = s[t - 1];
                    t -= 2;
                    /* 空字典，直接插入 */
                    if (size == 0) {
                        s[s[base(i.l, s, b) + i.a]]++;
                        s[current_root + 1] = key;
                        s[current_root + 2] = val;
                        break;
                    }
                    /* 非空字典，查找 */
                    while (s[current_root + 1] != key) {
                        if (key < s[current_root + 1]) {
                            if (s[current_root] == 0) break;
                            current_root = s[current_root];
                        }
                        else if (key > s[current_root + 1]) {
                            if (s[current_root + 3] == 0) break;
                            current_root = s[current_root + 3];
                        }
                        else {
                            break;
                        }
                    }

                    if (key < s[current_root + 1]) {
                        s[current_root] = alloc_map_node(s, avail, 0, key, val, 0);
                        s[s[base(i.l, s, b) + i.a]]++;
                    }
                    else if (key > s[current_root + 1]) {
                        s[current_root + 3] = alloc_map_node(s, avail, 0, key, val, 0);
                        s[s[base(i.l, s, b) + i.a]]++;
                    }
                    else {
                        s[current_root + 2] = val;
                    }
                }
                else { /* 集合插入 */
                    int val = s[t];
                    t -= 1;
                    /* 空集合，直接插入 */
                    if (size == 0) {
                        s[s[base(i.l, s, b) + i.a]]++;
                        s[current_root + 1] = val;
                        break;
                    }
                    /* 非空集合，查找 */
                    while (s[current_root + 1] != val) {
                        if (val < s[current_root + 1]) {
                            if (s[current_root] == 0) break;
                            current_root = s[current_root];
                        }
                        else if (val > s[current_root + 1]) {
                            if (s[current_root + 2] == 0) break;
                            current_root = s[current_root + 2];
                        }
                        else {
                            break;
                        }
                    }

                    if (val < s[current_root + 1]) {
                        s[current_root] = alloc_set_node(s, avail, 0, val, 0);
                        s[s[base(i.l, s, b) + i.a]]++;
                    }
                    else if (val > s[current_root + 1]) {
                        s[current_root + 2] = alloc_set_node(s, avail, 0, val, 0);
                        s[s[base(i.l, s, b) + i.a]]++;
                    }
                }
                }
                break;
            case delete: { /* 将栈顶值/键从字典/集合中删去 */
                int current_root = s[base(i.l, s, b) + i.a];
                int size = s[current_root];
                current_root += 2;
                if (s[current_root - 1] == 0) { /* 字典删除 */
                    int key = s[t];
                    t -= 1;
                    if (size == 0) {
                        break;
                    }
                    /* 找到待删除节点 */
                    int prev = 0;
                    while (s[current_root + 1] != key) {
                        if (key < s[current_root + 1]) {
                            if (s[current_root] == 0) break;
                            prev = current_root;
                            current_root = s[current_root];
                        }
                        else if (key > s[current_root + 1]) {
                            if (s[current_root + 3] == 0) break;
                            prev = current_root;
                            current_root = s[current_root + 3];
                        }
                        else {
                            break;
                        }
                    }
                    /* 节点存在 */
                    if (key == s[current_root + 1]) {
                        s[s[base(i.l, s, b) + i.a]]--;
                        /* 没有左子树或没有子树 */
                        if (s[current_root] == 0) {
                            int temp_node_ptr = s[current_root + 3]; /* 获取右子树 */
                            if (prev != 0) {
                                avail[current_root] = 0;
                                avail[current_root + 1] = 0;
                                avail[current_root + 2] = 0;
                                avail[current_root + 3] = 0;
                                if (s[prev] == current_root) {
                                    s[prev] = temp_node_ptr;
                                }
                                else {
                                    s[prev + 3] = temp_node_ptr;
                                }
                            }
                        }
                        /* 没有右子树 */
                        else if (s[current_root + 3] == 0) {
                            int temp_node_ptr = s[current_root]; /* 获取左子树 */
                            if (prev != 0) {
                                avail[current_root] = 0;
                                avail[current_root + 1] = 0;
                                avail[current_root + 2] = 0;
                                avail[current_root + 3] = 0;
                                if (s[prev] == current_root) {
                                    s[prev] = temp_node_ptr;
                                }
                                else {
                                    s[prev + 3] = temp_node_ptr;
                                }
                            }
                        }
                        /* 有两个子树 */
                        else {
                            /* 找到中序后继及其父节点 */
                            int next_prev = current_root;
                            int next_root = s[current_root + 3];
                            while (s[next_root] != 0) {
                                next_prev = next_root;
                                next_root = s[next_root];
                            }

                            /* 将中序后继的对复制到要删除的节点 */
                            s[current_root + 1] = s[next_root + 1];
                            s[current_root + 2] = s[next_root + 2];

                            /* 将后继节点的右子树连接到其父节点 */
                            if (next_prev == current_root) {
                                s[next_prev + 3] = s[next_root + 3];
                            }
                            else {
                                s[next_prev] = s[next_root + 3];
                            }
                            avail[next_root] = 0;
                            avail[next_root + 1] = 0;
                            avail[next_root + 2] = 0;
                            avail[next_root + 3] = 0;
                        }
                    }
                }
                else { /* 集合删除 */
                    int val = s[t];
                    t -= 1;
                    if (size == 0) {
                        break;
                    }
                    /* 找到待删除节点 */
                    int prev = 0;
                    while (s[current_root + 1] != val) {
                        if (val < s[current_root + 1]) {
                            if (s[current_root] == 0) break;
                            prev = current_root;
                            current_root = s[current_root];
                        }
                        else if (val > s[current_root + 1]) {
                            if (s[current_root + 2] == 0) break;
                            prev = current_root;
                            current_root = s[current_root + 2];
                        }
                        else {
                            break;
                        }
                    }
                    /* 节点存在 */
                    if (val == s[current_root + 1]) {
                        s[s[base(i.l, s, b) + i.a]]--;
                        /* 没有左子树或没有子树 */
                        if (s[current_root] == 0) {
                            int temp_node_ptr = s[current_root + 2]; /* 获取右子树 */
                            if (prev != 0) {
                                avail[current_root] = 0;
                                avail[current_root + 1] = 0;
                                avail[current_root + 2] = 0;
                                if (s[prev] == current_root) {
                                    s[prev] = temp_node_ptr;
                                }
                                else {
                                    s[prev + 2] = temp_node_ptr;
                                }
                            }
                        }
                        /* 没有右子树 */
                        else if (s[current_root + 2] == 0) {
                            int temp_node_ptr = s[current_root]; /* 获取左子树 */
                            if (prev != 0) {
                                avail[current_root] = 0;
                                avail[current_root + 1] = 0;
                                avail[current_root + 2] = 0;
                                if (s[prev] == current_root) {
                                    s[prev] = temp_node_ptr;
                                }
                                else {
                                    s[prev + 2] = temp_node_ptr;
                                }
                            }
                        }
                        /* 有两个子树 */
                        else {
                            /* 找到中序后继及其父节点 */
                            int next_prev = current_root;
                            int next_root = s[current_root + 2];
                            while (s[next_root] != 0) {
                                next_prev = next_root;
                                next_root = s[next_root];
                            }

                            /* 将中序后继的值复制到要删除的节点 */
                            s[current_root + 1] = s[next_root + 1];

                            /* 将后继节点的右子树连接到其父节点 */
                            if (next_prev == current_root) {
                                s[next_prev + 2] = s[next_root + 2];
                            }
                            else {
                                s[next_prev] = s[next_root + 2];
                            }
                            avail[next_root] = 0;
                            avail[next_root + 1] = 0;
                            avail[next_root + 2] = 0;
                        }
                    }
                }
                }
                break;
            case find: { /* 在字典/集合上查找栈顶值/键，将是否存在/值入栈 */
                int current_root = s[base(i.l, s, b) + i.a];
                int size = s[current_root];
                current_root += 2;
                if (s[current_root - 1] == 0) { /* 字典查找 */
                    if (s[t - 1] == 0) { /* 返回真值 */
                        int key = s[t];
                        t -= 2;
                        /* 空字典，返回0 */
                        if (size == 0) {
                            t += 1;
                            s[t] = 0;
                            break;
                        }
                        /* 非空字典，查找 */
                        while (s[current_root + 1] != key) {
                            if (key < s[current_root + 1]) {
                                if (s[current_root] == 0) break;
                                current_root = s[current_root];
                            }
                            else if (key > s[current_root + 1]) {
                                if (s[current_root + 3] == 0) break;
                                current_root = s[current_root + 3];
                            }
                            else {
                                break;
                            }
                        }

                        /* 节点存在 */
                        if (key == s[current_root + 1]) {
                            t += 1;
                            s[t] = 1;
                        }
                        else {
                            t += 1;
                            s[t] = 0;
                        }
                    }
                    else { /* 返回数值（如果不存在，返回0，即便0也是合法的value） */
                        int key = s[t];
                        t -= 2;
                        /* 空字典，返回0 */
                        if (size == 0) {
                            t += 1;
                            s[t] = 0;
                            break;
                        }
                        /* 非空字典，查找 */
                        while (s[current_root + 1] != key) {
                            if (key < s[current_root + 1]) {
                                if (s[current_root] == 0) break;
                                current_root = s[current_root];
                            }
                            else if (key > s[current_root + 1]) {
                                if (s[current_root + 3] == 0) break;
                                current_root = s[current_root + 3];
                            }
                            else {
                                break;
                            }
                        }

                        /* 节点存在 */
                        if (key == s[current_root + 1]) {
                            t += 1;
                            s[t] = s[current_root + 2];
                        }
                        else {
                            t += 1;
                            s[t] = 0;
                        }
                    }
                }
                else { /* 集合查找 */
                    int val = s[t];
                    t -= 1;
                    /* 空集合，返回0 */
                    if (size == 0) {
                        t += 1;
                        s[t] = 0;
                        break;
                    }
                    /* 非空集合，查找 */
                    while (s[current_root + 1] != val) {
                        if (val < s[current_root + 1]) {
                            if (s[current_root] == 0) break;
                            current_root = s[current_root];
                        }
                        else if (val > s[current_root + 1]) {
                            if (s[current_root + 2] == 0) break;
                            current_root = s[current_root + 2];
                        }
                        else {
                            break;
                        }
                    }

                    /* 节点存在 */
                    if (val == s[current_root + 1]) {
                        t += 1;
                        s[t] = 1;
                    }
                    else {
                        t += 1;
                        s[t] = 0;
                    }
                }
                }
                break;
            case traverse: { /* 中序遍历字典/集合，将遍历结果存入数组，数组起始地址压栈 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }

                int current_root = s[base(i.l, s, b) + i.a];
                int size = s[current_root];
                current_root += 2;

                /* 空集合或字典 */
                if (size == 0) {
                    t += 1; /* 压栈一个空指针 */
                    s[t] = 0;
                    break;
                }

                if (s[current_root - 1] == 0) { /* 字典遍历 */
                    int array_alloc_size = 2 + 2 * size;
                    int array_start_ptr = try_alloc(array_alloc_size, avail);
                    if (array_start_ptr == 0) {
                        printf("Error: Heap overflow for set traverse array\n");
                        fprintf(fresult, "Error: Heap overflow for set traverse array\n");
                        exit(1);
                    }

                    s[array_start_ptr] = array_alloc_size;
                    s[array_start_ptr + 1] = 1;

                    array_start_ptr += 2;
                    int index = array_start_ptr;
                    collect_map_values(current_root, s, &index);

                    t += 1;
                    s[t] = array_start_ptr;
                }
                else { /* 集合遍历 */
                    int array_alloc_size = 2 + size;
                    int array_start_ptr = try_alloc(array_alloc_size, avail);
                    if (array_start_ptr == 0) {
                        printf("Error: Heap overflow for set traverse array\n");
                        fprintf(fresult, "Error: Heap overflow for set traverse array\n");
                        exit(1);
                    }

                    s[array_start_ptr] = array_alloc_size;
                    s[array_start_ptr + 1] = 1;

                    array_start_ptr += 2;
                    int index = array_start_ptr;
                    collect_set_values(current_root, s, &index);

                    t += 1;
                    s[t] = array_start_ptr;
                }
            }
            break;
            case size: { /* 返回字典/集合的大小 */
                if (t + 1 >= stacksize) {
                    printf("Stack overflow at line %d\n", p - 1);
                    fprintf(fresult, "Stack overflow at line %d\n", p - 1);
                    exit(1);
                }
                int current_root = s[base(i.l, s, b) + i.a];
                t = t + 1;
                s[t] = s[current_root];
            }
            break;
        }

        // Sleep(500);
        // printf("after %d___________________________________________\n", p);
        //     for(int i = 500; i <= 530; i++) {
        //         printf("%d, ", s[i]);
        //     }
        // printf("\n___________________________________________\n");
    } while (p != 0);

    printf("End pl0\n");
    fprintf(fresult, "End pl0\n");
}

/* 通过过程基址求上l层过程的基址 */
int base(int l, int* s, int b){
    int b1;
    b1 = b;
    while (l > 0){
        b1 = s[b1];
        l--;
    }
    return b1;
}