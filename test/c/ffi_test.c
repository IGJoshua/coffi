#include <stdio.h>

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

int counter = 0;

static char* responses[] = { "Hello, world!", "Goodbye friend.", "co'oi prenu" };

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
