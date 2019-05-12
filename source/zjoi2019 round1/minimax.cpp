// =================================
//   author: memset0
//   date: 2019.05.02 13:23:23
//   website: https://memset0.cn/
// =================================
#include <bits/stdc++.h>
#define FLAG_F 1926
#define FLAG_G  817
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
template <class T> inline void print(T *a, int l, int r, std::string s = "") {
    if (s != "") std::cout << s << ": ";
    for (int i = l; i <= r; i++) print(a[i], " \n"[i == r]);
}

const int N = 2e5 + 10, inf = 1e9, mod = 998244353, half = (mod + 1) >> 1;
bool is_leaf[N];
int fa[N], dep[N];
int n, w, c, m, L, R, LL, RR;
std::vector<int> todo1[N], todo2[N];
int tot = 2, hed[N], to[N << 1], nxt[N << 1];
int f[N], g[N], ans[N], sum[N], son[N], siz[N];

inline int dec(int a, int b) { a -= b; return a < 0 ? a + mod : a; }
inline int inc(int a, int b) { a += b; return a >= mod ? a - mod : a; }
inline int mul(int a, int b) { return (ll)a * b - (ll)a * b / mod * mod; }
inline int inv(int x) { return x < 2 ? 1 : mul(mod - mod / x, inv(mod % x)); }
inline int fpow(int a, int b) { int s = 1; for (; b; b >>= 1, a = mul(a, a)) if (b & 1) s = mul(s, a); return s; }

namespace bst {
    int rt, top, fa[N], stk[N], ch[N][2], bottom[N];

    struct multinum {
        int v, z;
        inline int get() { return z ? 0 : v; }
        inline void operator*=(int p) { p ? v = mul(v, p) : ++z; }
        inline void operator/=(int p) { p ? v = mul(v, inv(p)) : --z; }
        inline void init(int x) { x ? (v = x, z = 0) : (v = 0, z = 1); }
    } lf[N], lg[N];

    struct matrix {
        int a[3][3];
        inline void out() {
            printf("[%d %d %d] [%d %d %d] [%d %d %d]\n",
                a[0][0], a[0][1], a[0][2],
                a[1][0], a[1][1], a[1][2],
                a[2][0], a[2][1], a[2][2]);
        }
    } f[N], g[N], h[N];
    inline matrix operator*(const matrix &a, const matrix &b) {
        matrix c; memset(c.a, 0, sizeof(c.a));
        for (register int i = 0; i < 3; i++)
            for (register int j = 0; j < 3; j++)
                for (register int k = 0; k < 3; k++)
                    c.a[i][j] = (c.a[i][j] + (ll)a.a[i][k] * b.a[k][j]) % mod;
        return c;
    }

    inline void get_value(int x, int &F, int &G) {
        matrix it = f[x] * h[bottom[x]];
        F = it.a[0][0], G = it.a[1][0];
    }

    inline void get_value_with_backup(int x, int &F, int &G, const matrix &backup) {
        matrix it = f[x] * backup;
        F = it.a[0][0], G = it.a[1][0];
    }

    inline void maintain(int u) {
        f[u] = g[u];
        if (ch[u][0]) f[u] = f[ch[u][0]] * f[u];
        if (ch[u][1]) f[u] = f[u] * f[ch[u][1]];
    }	

    int build(int l, int r) {
        // printf("build %d %d => ", l, r), print(stk, l, r);
        int sum = 0, tmp = 0;
        for (int i = l; i <= r; i++) sum += siz[stk[i]] - siz[son[stk[i]]];
        for (int i = l; i <= r; i++) {
            tmp += siz[stk[i]] - siz[son[stk[i]]];
            if ((tmp << 1) >= sum) {
                fa[ch[stk[i]][0] = build(l, i - 1)] = stk[i];
                fa[ch[stk[i]][1] = build(i + 1, r)] = stk[i];
                return maintain(stk[i]), stk[i];
            }
        }
        return 0;
    }

    inline int build(int u) {
        top = 0;
        for (int x = u; x; x = son[x]) stk[++top] = x;
        h[stk[top]].a[0][0] = ringo::f[stk[top]];
        h[stk[top]].a[1][0] = ringo::g[stk[top]];
        h[stk[top]].a[2][0] = 1;
        for (int i = 1; i <= top; i++) {
            bottom[stk[i]] = stk[top];
            if (i == top) {
                g[stk[i]].a[0][0] = 1;
                g[stk[i]].a[1][1] = 1;
                g[stk[i]].a[2][2] = 1;
            } else {
                lf[stk[i]].init(mod - 1);
                lg[stk[i]].init(mod - 1);
                for (int j = hed[stk[i]], v; v = to[j], j; j = nxt[j])
                    if (v != ringo::fa[stk[i]] && v != ringo::son[stk[i]]) {
                        // printf("%d <- %d : %d %d\n", u, v, ringo::f[v], ringo::g[v]);
                        lf[stk[i]] *= ringo::f[v];
                        lg[stk[i]] *= ringo::g[v];
                    }
                // printf(">> %d : [%d %d] [%d %d]\n", stk[i], lf[stk[i]].v, lf[stk[i]].z, lg[stk[i]].v, lg[stk[i]].z);
                g[stk[i]].a[0][0] = lf[stk[i]].get();
                g[stk[i]].a[1][1] = lg[stk[i]].get();
                g[stk[i]].a[0][2] = 1;
                g[stk[i]].a[1][2] = 1;
                g[stk[i]].a[2][2] = 1;
            }
        }
        // printf("==> ");
        // for (int i = 1; i <= top; i++) print(stk[i], " \n"[i == top]);
        return build(1, top);
    }

    inline void modify(int s, int flag) {
        // printf("modify %d %s\n", s, flag == FLAG_F ? "F" : "G");
        matrix backup = h[s];
        for (int x = s, now_f, now_g; x; x = fa[x]) {
            // printf("  >> %d : %d\n", x, ch[fa[x]][0] != x && ch[fa[x]][1] != x && fa[x]);
            if (ch[fa[x]][0] != x && ch[fa[x]][1] != x && fa[x]) {
                if (bottom[x] == s) get_value_with_backup(x, now_f, now_g, backup);
                else get_value(x, now_f, now_g);
                // printf("%d <- %d : [-] %d %d\n", fa[x], x, now_f, now_g);
                lf[fa[x]] /= now_f;
                lg[fa[x]] /= now_g;
                // printf(">>>> [%d %d] [%d %d]\n", lf[fa[x]].v, lf[fa[x]].z, lg[fa[x]].v, lg[fa[x]].z);
            }
            if (x == s) {
                if (flag == FLAG_F) h[x].a[0][0] = half;
                if (flag == FLAG_G) h[x].a[1][0] = half;
            } else {
                g[x].a[0][0] = lf[x].get();
                g[x].a[1][1] = lg[x].get();
            }
            maintain(x);
            if (ch[fa[x]][0] != x && ch[fa[x]][1] != x && fa[x]) {
                get_value(x, now_f, now_g);
                // printf("%d <- %d : [+] %d %d\n", fa[x], x, now_f, now_g);
                lf[fa[x]] *= now_f;
                lg[fa[x]] *= now_g;
                // printf(">>>> [%d %d] [%d %d]\n", lf[fa[x]].v, lf[fa[x]].z, lg[fa[x]].v, lg[fa[x]].z);
            }
        }
    }

}

int init_dfs(int u) {
    siz[u] = 1;
    int res = dep[u] & 1 ? -inf : inf;
    for (int i = hed[u], v; v = to[i], i; i = nxt[i])
        if (v != fa[u]) {
            fa[v] = u, dep[v] = dep[u] + 1;
            res = dep[u] & 1 ? std::max(res, init_dfs(v)) : std::min(res, init_dfs(v));
            siz[u] += siz[v];
            if (siz[v] > siz[son[u]]) son[u] = v;
        }
    if (res == inf || res == -inf) {
        m += is_leaf[u] = true;
        return u;
    }
    return res;
}

int build(int x) {
    for (int u = x; u; u = son[u])
        for (int i = hed[u], v; v = to[i], i; i = nxt[i])
            if (v != fa[u] && v != son[u])
                bst::fa[build(v)] = u;
    return bst::build(x);
}

inline void solve(int u) {
    if (is_leaf[u]) return;
    f[u] = g[u] = 1;
    for (int i = hed[u], v; v = to[i], i; i = nxt[i])
        if (v != fa[u]) {
            solve(v);
            f[u] = mul(f[u], f[v]);
            g[u] = mul(g[u], g[v]);
        }
    f[u] = dec(1, f[u]);
    g[u] = dec(1, g[u]);
}

void main() {
    read(n), read(L), read(R);
    LL = std::max(L - 1, 1), RR = std::max(R, n - 1);
    for (int u, v, i = 1; i < n; i++) {
        read(u), read(v);
        nxt[tot] = hed[u], to[tot] = v, hed[u] = tot++;
        nxt[tot] = hed[v], to[tot] = u, hed[v] = tot++;
    }
    
    dep[1] = 1, w = init_dfs(1);
    for (int c, u = 1; u <= n; u++) if (is_leaf[u]) {
        if (u > w) {
            c = u - w + 1;
            f[u] = 1, g[u] = 0;
            if (LL <= c && c <= RR) todo1[c].push_back(u);
            else if (c < LL) g[u] = half;
        } else if (u < w) {
            c = w - u + 1;
            g[u] = 1, f[u] = 0;
            if (LL <= c && c <= RR) todo2[c].push_back(u);
            else if (c < LL) f[u] = half;
        } else if (u == w) {
            f[u] = g[u] = 0;
        }
        (dep[u] & 1) ? g[u] = dec(1, g[u]) : f[u] = dec(1, f[u]);
    }
    solve(1);
    bst::rt = build(1);
    bst::get_value(bst::rt, f[1], g[1]);
    // for (int i = 1; i <= n; i++) {
    // 	printf("F %d ", i), bst::f[i].out();
    // 	printf("G %d ", i), bst::g[i].out();
    // 	printf("H %d ", i), bst::h[i].out();
    // }
    // for (int i = 1; i <= n; i++) printf("[%d %d]%c", bst::lf[i].v, bst::lf[i].z, " \n"[i == n]);
    // for (int i = 1; i <= n; i++) printf("[%d %d]%c", bst::lg[i].v, bst::lg[i].z, " \n"[i == n]);
    for (int F, G, i = std::max(L - 1, 1); i <= std::min(R, n - 1); i++) {
        for (std::vector<int>::iterator it = todo1[i].begin(); it != todo1[i].end(); it++)
            bst::modify(*it, FLAG_G);
        for (std::vector<int>::iterator it = todo2[i].begin(); it != todo2[i].end(); it++)
            bst::modify(*it, FLAG_F);
        bst::get_value(bst::rt, F, G);
        sum[i] = mul(dec(1, mul(dec(1, F), G)), fpow(2, m - 1));
        // printf("%d => %d : %d %d\n", i, sum[i], F, G);
        // for (int i = 1, F, G; i <= n; i++) bst::get_value(i, F, G), print(F, " \n"[i == n]);
        // for (int i = 1, F, G; i <= n; i++) bst::get_value(i, F, G), print(G, " \n"[i == n]);
    }

    for (int i = L; i <= R; i++) ans[i] = dec(sum[i], sum[i - 1]);
    ans[1] = inc(sum[1], fpow(2, m - 1));
    ans[n] = dec(dec(fpow(2, m - 1), 1), sum[n - 1]);
    print(ans, L, R);
    // print(ans, 1, n, "ans"), print(sum, 1, n, "sum");
}

} signed main() {
#ifdef MEMSET0_LOCAL_ENVIRONMENT
    freopen("1.in", "r", stdin);
    // freopen("1.out", "w", stdout);
#endif
    return ringo::main(), 0;
}