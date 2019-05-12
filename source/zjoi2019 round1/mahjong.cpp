// =================================
//   author: memset0
//   date: 2019.05.01 15:29:37
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

const int N = 110, M = 410, R = 2100, mod = 998244353;
int n, m, res, a[N], ans[M], fac[M], ifac[M];

inline void sub_d(int &a, int b) { a += b; if (a >= mod) a -= mod; }
inline int dec(int a, int b) { a -= b; return a < 0 ? a + mod : a; }
inline int sub(int a, int b) { a += b; return a >= mod ? a - mod : a; }
inline int mul(int a, int b) { return (ll)a * b - (ll)a * b / mod * mod; }
inline int inv(int x) { return x < 2 ? 1 : mul(mod - mod / x, inv(mod % x)); }
inline int C(int a, int b) { return a < b ? 0 : mul(fac[a], mul(ifac[b], ifac[a - b])); }

namespace am {
    struct matrix {
        int a[3][3];
        inline matrix() {
            a[0][0] = a[0][1] = a[0][2] = -1;
            a[1][0] = a[1][1] = a[1][2] = -1;
            a[2][0] = a[2][1] = a[2][2] = -1;
        }
        inline void out() {
            for (register int i = 0; i < 3; i++) {
                putchar('[');
                for (register int j = 0; j < 3; j++)
                    print(a[i][j], j == 2 ? ']' : ' ');
                putchar(' ');
            }
            putchar('\n');
        }

        inline void operator += (const matrix &other) {
            for (register int i = 0; i < 3; i++)
                for (register int j = 0; j < 3; j++)
                    a[i][j] = std::max(a[i][j], other.a[i][j]);
        }
        inline bool operator == (const matrix &other) const {
            for (register int i = 0; i < 3; i++)
                for (register int j = 0; j < 3; j++)
                    if (a[i][j] != other.a[i][j])
                        return false;
            return true;
        }
        inline bool operator < (const matrix &other) const {
            for (register int i = 0; i < 3;i++)
                for (register int j = 0; j < 3; j++)
                    if (a[i][j] != other.a[i][j])
                        return a[i][j] < other.a[i][j];
            return false;
        }

        inline matrix get_move(int x) {
            matrix res;
            for (register int i = 0; i < 3; i++)
                for (register int j = 0; j < 3; j++) if (~a[i][j])
                    for (register int k = 0; i + j + k <= x && k < 3; k++) {
                        res.a[j][k] = std::max(res.a[j][k], std::min(a[i][j] + i + ((x - i - j - k) >= 3), 4));
                    }
            return res;
        }
    };

    struct node {
        matrix f[2];
        int pair, ch[5];
        inline node(int v = 0) {
            pair = v, ch[0] = ch[1] = ch[2] = ch[3] = ch[4] = 0;
        }
        inline bool operator < (const node &other) const {
            if (pair != other.pair) return pair < other.pair;
            return f[0] == other.f[0] ? f[1] < other.f[1] : f[0] < other.f[0];
        }
        inline void out() {
            printf("%d : %d %d %d %d %d\n", pair, ch[0], ch[1], ch[2], ch[3], ch[4]);
            putchar(' '), putchar(' '), putchar(' '), f[0].out();
            putchar(' '), putchar(' '), putchar(' '), f[1].out();
        }

        inline bool is_hu() {
            if (pair >= 7) return true;		
            for (register int i = 0; i < 3; i++)
                for (register int j = 0; j < 3; j++)
                    if (f[1].a[i][j] >= 4)
                        return true;
            return false;
        }

        static inline node get_hu() { return node(-1); }
        static inline node get_init() {
            node res;
            res.f[0].a[0][0] = 0;
            return res;
        }

        inline node get_move(int k) {
            if (!~pair) return get_hu();
            node res;
            res.pair = pair + (k >= 2);
            res.f[0] = f[0].get_move(k);
            res.f[1] = f[1].get_move(k);
            if (k >= 2) res.f[1] += f[0].get_move(k - 2);
            return res.is_hu() ? get_hu() : res;
        }
    } p[R];

    int tot;
    std::map<node, int> map;

    inline void expand(int x) {
        node &u = p[x];
        for (int i = 0; i <= 4; i++) {
            node v = u.get_move(i);
            std::map<node, int>::iterator it = map.find(v);
            if (it == map.end()) {
                p[++tot] = v;
                map[v] = u.ch[i] = tot;
                // printf(">> tot = %d\n", tot);
            } else {
                u.ch[i] = it->second;
            }
        }
    }

    void build() {
        p[++tot] = node::get_hu(), map[p[tot]] = tot;
        p[++tot] = node::get_init(), map[p[tot]] = tot;
        for (int i = 2; i <= tot; i++) expand(i);
        // std::cout << tot << std::endl;
        // for (int i = 1; i <= tot; i++) p[i].out();
    }
}

int dp[2][R][M];
int solve() {
    int (*f)[M] = dp[0];
    int (*g)[M] = dp[1];
    f[2][0] = 1;
    for (int i = 1; i <= n; i++) {
        for (int j = 2; j <= am::tot; j++)
            for (int k = 0; k <= m; k++)
                g[j][k] = 0;
        for (int j = 2; j <= am::tot; j++)
            for (int k = 0; k <= m; k++) if (f[j][k])
                for (int p = 0; p + a[i] <= 4 && p + k <= m; p++) {
                    sub_d(g[am::p[j].ch[p + a[i]]][k + p], mul(f[j][k], C(4 - a[i], p)));
                    // printf("%d : [%d %d] -> [%d %d] : %d\n", i, j, k, am::p[j].ch[p + a[i]], k + p, mul(f[j][k], C(4 - a[i], p)));
                }
        std::swap(f, g);
    }
    for (int i = 2; i <= am::tot; i++)
        for (int j = 0; j <= m; j++)
            sub_d(ans[j], f[i][j]);
    // for (int i = 0; i <= m; i++)
    // 	print(ans[i], " \n"[i == m]);
    for (int i = 1; i <= m; i++)
        sub_d(res, mul(ans[i], mul(fac[i], fac[m - i])));
    return sub(mul(res, ifac[m]), 1);
}

void main() {
    am::build();
    read(n), m = (n << 2) - 13;
    fac[0] = fac[1] = ifac[0] = ifac[1] = 1;
    for (int i = 2; i <= m; i++) fac[i] = mul(fac[i - 1], i);
    for (int i = 2; i <= m; i++) ifac[i] = mul(mod - mod / i, ifac[mod % i]);
    for (int i = 2; i <= m; i++) ifac[i] = mul(ifac[i - 1], ifac[i]);
    for (int i = 1, x, y; i <= 13; i++)
        read(x), read(y), a[x]++;
    print(solve(), '\n');
}

} signed main() {
#ifdef MEMSET0_LOCAL_ENVIRONMENT
    freopen("1.in", "r", stdin);
    // freopen("1.out", "w", stdout);
#endif
    return ringo::main(), 0;
}