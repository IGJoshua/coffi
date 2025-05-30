#include <stdio.h>
#include <stdlib.h>

const int c = 42;
const char *s = "Test string";

int add_numbers(int a, int b) {
    return a + b;
}

typedef struct point {
    float x;
    float y;
} Point;

Point add_points(Point a, Point b) {
    Point res = {};

    res.x = a.x + b.x;
    res.y = a.y + b.y;

    return res;
}

typedef char *CString;

typedef CString (*StringFactory)(void);

CString upcall_test(StringFactory fun) {
    return fun();
}

int upcall_test2(int (*f)(void)) {
    return f();
}

char *mut_str = NULL;
int counter = 0;

static char* responses[] = { "Hello, world!", "Goodbye friend.", "co'oi prenu" };

char* upcall_test_int_fn_string_ret(int (*f)(void)) {
    return responses[f()];
}

CString get_string1(void) {
    return responses[counter++ % 3];
}

CString get_string2(void) {
    return "Alternate string";
}

StringFactory get_downcall(int whichString) {
    switch (whichString % 2) {
    case 0:
        return get_string1;
    case 1:
        return get_string2;
    default:
        return 0;
    }
}

typedef struct alignment_test {
    char a;
    double x;
    float y;
} AlignmentTest;

AlignmentTest get_struct() {
    AlignmentTest ret = {};
    ret.a = 'x';
    ret.x = 3.14;
    ret.y = 42.0;

    return ret;
}

void test_call_with_trailing_string_arg(int a, int b, char* text) {
    printf("call of `test_call_with_trailing_string_arg` with a=%i b=%i text='%s'",1,2,text);
    printf("\r                                                                          ");
    return;
}

int freed = 0;

int get_variable_length_array(float **arr) {
    freed = 0;
    *arr = malloc(sizeof(float) * 7);

    for (int i = 0; i < 7; ++i) {
        (*arr)[i] = 1.5f * i;
    }

    return 7;
}

void free_variable_length_array(float *arr) {
    freed = 1;
    free(arr);
}

typedef struct complextype {
    Point x;
    char  y;
    int   z[4];
    char *w;
} ComplexType;

ComplexType complexTypeTest(ComplexType a) {
    ComplexType ret = {};
    ret.x = a.x;
    ret.x.x++;
    ret.x.y++;
    ret.y = a.y-1;
    ret.z[0] = a.z[0];
    ret.z[1] = a.z[1];
    ret.z[2] = a.z[2];
    ret.z[3] = a.z[3];
    ret.w = "hello from c";
    return ret;
}

int is_42(int **arg) {
    return **arg == 42;
}
