// =================================
//   author: memset0
//   website: https://memset0.cn/
// =================================
#include <bits/stdc++.h>
#define ll long long
#define debug(...) ((void)0)
#ifndef debug
#define debug(...) fprintf(stderr,__VA_ARGS__)
#endif
namespace ringo {
template <class T> inline void read(T &x) {
    x = 0; register char c = getchar(); register bool f = 0;
    while (!isdigit(c)) f ^= c == '-', c = getchar();
    while (isdigit(c)) x = x * 10 + c - '0', c = getchar();
    if (f) x = -x;
}
template <class T> inline void print(T x) {
    if (x < 0) putchar('-'), x = -x;
    if (x > 9) print(x / 10);
    putchar('0' + x % 10);
}
template <class T> inline void print(T x, char c) { print(x), putchar(c); }

const int N = 1e5 + 10, L = 3, mod = 998244353;
int n, m, ans;

inline int dec(int a, int b) { a -= b; return a < 0 ? a + mod : a; }
inline int sub(int a, int b) { a += b; return a >= mod ? a - mod : a; }
inline int mul(int a, int b) { return (ll)a * b - (ll)a * b / mod * mod; } 

inline int sub(int a, int b, int c) { return sub(a, sub(b, c)); }
inline int half(int x) { return x & 1 ? (x + mod) >> 1 : x >> 1; }

struct matrix {
#define f0 a[0][0]
#define f1 a[1][0]
#define f2 a[2][0]
    int a[L][L];
    inline matrix() {}
    inline matrix(char c) { memset(a, c, sizeof(a)); }
    inline matrix(int x, int y, int z) { a[0][0] = x, a[1][0] = y, a[2][0] = z; }
    inline void out() const {
        for (int i = 0; i < 3; i++)
            printf("{%d %d %d}%c", a[i][0], a[i][1], a[i][2], " \n"[i == 2]);
    }
    friend inline matrix operator * (const matrix &a, const matrix &b) {
        matrix c;
        c.a[0][0] = ((ll)a.a[0][0] * b.a[0][0] + (ll)a.a[0][1] * b.a[1][0] + (ll)a.a[0][2] * b.a[2][0]) % mod;
        c.a[0][1] = ((ll)a.a[0][0] * b.a[0][1] + (ll)a.a[0][1] * b.a[1][1] + (ll)a.a[0][2] * b.a[2][1]) % mod;
        c.a[0][2] = ((ll)a.a[0][0] * b.a[0][2] + (ll)a.a[0][1] * b.a[1][2] + (ll)a.a[0][2] * b.a[2][2]) % mod;
        c.a[1][0] = ((ll)a.a[1][0] * b.a[0][0] + (ll)a.a[1][1] * b.a[1][0] + (ll)a.a[1][2] * b.a[2][0]) % mod;
        c.a[1][1] = ((ll)a.a[1][0] * b.a[0][1] + (ll)a.a[1][1] * b.a[1][1] + (ll)a.a[1][2] * b.a[2][1]) % mod;
        c.a[1][2] = ((ll)a.a[1][0] * b.a[0][2] + (ll)a.a[1][1] * b.a[1][2] + (ll)a.a[1][2] * b.a[2][2]) % mod;
        c.a[2][0] = ((ll)a.a[2][0] * b.a[0][0] + (ll)a.a[2][1] * b.a[1][0] + (ll)a.a[2][2] * b.a[2][0]) % mod;
        c.a[2][1] = ((ll)a.a[2][0] * b.a[0][1] + (ll)a.a[2][1] * b.a[1][1] + (ll)a.a[2][2] * b.a[2][1]) % mod;
        c.a[2][2] = ((ll)a.a[2][0] * b.a[0][2] + (ll)a.a[2][1] * b.a[1][2] + (ll)a.a[2][2] * b.a[2][2]) % mod;
        return c;
    }
    inline matrix move0() { return matrix(sub(f0, half(f1), half(f2)), half(f1), half(f2)); }
    inline matrix move1() { return matrix(half(f0), half(f1), sub(half(f0), half(f1), f2)); }
    inline matrix move2() { return matrix(sub(f0, half(f1)), half(f1), f2); }
#undef f0
#undef f1
#undef f2
} I, A, pow[N];

void matrix_init() {
    for (int i = 0; i < L; i++) I.a[i][i] = 1;
    pow[0] = I;
    A.a[0][0] = 1, A.a[0][1] = 0, A.a[0][2] =  0;
    A.a[1][0] = 0, A.a[1][1] = 1, A.a[1][2] = (mod + 1) >> 1;
    A.a[2][0] = 0, A.a[2][1] = 0, A.a[2][2] = (mod + 1) >> 1;
    for (int i = 1; i <= m; i++) pow[i] = pow[i - 1] * A;
}

struct node {
    int l, r, mid, tag;
    matrix x;
} p[N << 2];

void move0(int u) {
//	printf(">> move0 %d <= %d %d %d\n", u, p[u].x.a[0][0], p[u].x.a[1][0], p[u].x.a[2][0]);
    ans = dec(ans, p[u].x.a[0][0]);
    p[u].x = p[u].x.move0();
    ans = sub(ans, p[u].x.a[0][0]);
//	printf(">> move0 %d => %d %d %d\n", u, p[u].x.a[0][0], p[u].x.a[1][0], p[u].x.a[2][0]);
}

void move1(int u) {
//	printf(">> move1 %d <= %d %d %d\n", u, p[u].x.a[0][0], p[u].x.a[1][0], p[u].x.a[2][0]);
    ans = dec(ans, p[u].x.a[0][0]);
    p[u].x = p[u].x.move1();
    ans = sub(ans, p[u].x.a[0][0]);
//	printf(">> move1 %d => %d %d %d\n", u, p[u].x.a[0][0], p[u].x.a[1][0], p[u].x.a[2][0]);
}

void move2(int u) {
//	printf(">> move2 %d <= %d %d %d\n", u, p[u].x.a[0][0], p[u].x.a[1][0], p[u].x.a[2][0]);
    ans = dec(ans, p[u].x.a[0][0]);
    p[u].x = p[u].x.move2();
    ans = sub(ans, p[u].x.a[0][0]);
//	printf(">> move2 %d => %d %d %d\n", u, p[u].x.a[0][0], p[u].x.a[1][0], p[u].x.a[2][0]);
}

void pushup(int u, int k) {
//	printf("pushup %d %d\n", u, k);
    p[u].tag += k;
    ans = dec(ans, p[u].x.a[0][0]);
    p[u].x = pow[k] * p[u].x;
    ans = sub(ans, p[u].x.a[0][0]);
}

void pushdown(int u) {
    if (p[u].l == p[u].r) return;
    pushup(u << 1, p[u].tag);
    pushup(u << 1 | 1, p[u].tag);
    p[u].tag = 0;
}

void build(int u, int l, int r) {
    p[u].l = l, p[u].r = r, p[u].mid = (l + r) >> 1;
    p[u].x.a[2][0] = 1;
    if (l == r) { return; }
    build(u << 1, l, p[u].mid);
    build(u << 1 | 1, p[u].mid + 1, r);
}

void modify(int u, int l, int r) {
//	printf(">> modify %d %d %d [%d %d]\n", u, l, r, p[u].l, p[u].r);
    pushdown(u);
    if (p[u].l == l && p[u].r == r) {
        move0(u);
        if (p[u].l != p[u].r) {
            pushup(u << 1, 1);
            pushup(u << 1 | 1, 1);
        }
        return;
    }
    move1(u);
    if (r <= p[u].mid) {
        modify(u << 1, l, r);
        move2(u << 1 | 1);
    } else if (l > p[u].mid) {
        modify(u << 1 | 1, l, r);
        move2(u << 1);
    } else {
        modify(u << 1, l, p[u].mid);
        modify(u << 1 | 1, p[u].mid + 1, r);
    }
}

void dfs(int u) {
    pushdown(u);
    if (p[u].l == p[u].r) return;
    dfs(u << 1), dfs(u << 1 | 1);
}

void main() {
    read(n), read(m);
    matrix_init();
    build(1, 1, n);
    for (int i = 1, l, r, opt, times = 1; i <= m; i++)
        if (read(opt), opt == 1) {
            read(l), read(r);
            modify(1, l, r);
            times = sub(times, times);
        } else {
//			for (int i = 1; i <= (n << 2); i++) if (p[i].mid) {
//				printf("[%d %d] %d %d %d\n", p[i].l, p[i].r, p[i].x.a[0][0], p[i].x.a[1][0], p[i].x.a[2][0]);
//			}
//			printf(">> %d * %d = %d\n", ans, times, mul(ans, times));
            print(mul(ans, times), '\n');
        }
}

} signed main() {
#ifdef MEMSET0_LOCAL_ENVIRONMENT
    freopen("1.in", "r", stdin);
#endif
    return ringo::main(), 0;
}