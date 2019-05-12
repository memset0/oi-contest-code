// =================================
//   author: memset0
//   date: 2019.04.08 17:56:11
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

const int N = 7e4 + 10;
int n, k, L, x, y, mod;
int c[N], w[N], ans[N];

inline int dec(int a, int b) { a -= b; return a < 0 ? a + mod : a; }
inline int sub(int a, int b) { a += b; return a >= mod ? a - mod : a; }
inline int mul(int a, int b) { return (ll)a * b - (ll)a * b / mod * mod; }
inline int inv(int x) { return x < 2 ? 1 : mul(mod - mod / x, inv(mod % x)); }
template <class T> inline int get_omega(T x) { x = x % k; return w[x < 0 ? x + k : x]; }
inline int fpow(int a, int b) { int s = 1; for (; b; b >>= 1, a = mul(a, a)) if (b & 1) s = mul(s, a); return s; }

int get_root(int p) {
    int phi = 1, tmp = p;
    for (int i = 2; i * i <= tmp; i++)
        if (tmp % i == 0) {
            phi *= i - 1, tmp /= i;
            while (tmp % i == 0) phi *= i, tmp /= i;
        }
    if (tmp != 1) phi *= tmp - 1;
    std::vector <int> e(1, 1);
    for (int i = 2; i * i <= phi; i++)
        if (phi % i == 0) {
            e.push_back(i);
            if (i * i != phi) e.push_back(phi / i);
        }
    for (int w = 2; ; w++) {
        bool flag = true;
        for (std::vector <int> ::iterator it = e.begin(); it != e.end(); it++)
            if (fpow(w, *it) == 1) {
                flag = false;
                break;
            }
        if (flag) return w;
    }
}

struct matrix {
    int a[3][3];
    inline void out() const {
        for (register int i = 0; i < 3; i++)
            for (register int j = 0; j < 3; j++)
                print(a[i][j], " \n"[j == 2]);
        puts("");
    }
    friend inline matrix operator + (matrix a, const matrix &b) {
        for (register int i = 0; i < 3; i++)
            for (register int j = 0; j < 3; j++)
                a.a[i][j] = sub(a.a[i][j], b.a[i][j]);
        return a;
    }
    friend inline matrix operator * (matrix a, int b) {
        for (register int i = 0; i < 3; i++)
            for (register int j = 0; j < 3; j++)
                a.a[i][j] = mul(a.a[i][j], b);
        return a;
    }
    friend inline matrix operator * (const matrix &a, const matrix &b) {
        matrix c; memset(c.a, 0, sizeof(c.a));
        for (register int i = 0; i < 3; i++)
            for (register int j = 0; j < 3; j++)
                for (register int k = 0; k < 3; k++)
                    c.a[i][j] = (c.a[i][j] + (ll)a.a[i][k] * b.a[k][j]) % mod;
        return c;
    }
    friend inline matrix fpow(matrix a, int b) {
        matrix s; memset(s.a, 0, sizeof(s.a)), s.a[0][0] = s.a[1][1] = s.a[2][2] = 1;
        for (; b; b >>= 1, a = a * a)
            if (b & 1) s = s * a;
        return s;
    }
} I, S, A;

struct poly : std::vector <int> {
    using std::vector <int> ::vector;
} f, g;

namespace MTT {
    const int M = N << 2;
    const double pi = acos(-1);
    ll p30 = 1ll << 30, p15 = 1ll << 15;
    struct complex {
        double a, b;
        inline complex() {}
        inline complex(double x) { a = x, b = 0; }
        inline complex(double x, double y) { a = x, b = y; }
        inline complex operator + (const complex &other) const { return complex(a + other.a, b + other.b); }
        inline complex operator - (const complex &other) const { return complex(a - other.a, b - other.b); }
        inline complex operator * (const complex &other) const { return complex(a * other.a - b * other.b, a * other.b + b * other.a); }
    } w[M], iw[M], a[M], b[M], c[M], d[M], e[M], f[M], g[M], h[M];
    int lim, rev[M];
    inline int init(int len) {
        int lim = 1, k = 0; while (lim < len) lim <<= 1, ++k;
        for (int i = 0; i < lim; i++) rev[i] = (rev[i >> 1] >> 1) | ((i & 1) << (k - 1));
        len = lim >> 1;
        for (int i = 0; i < len; i++) w[i + len] = complex(cos(pi * i / len), sin(pi * i / len));
        for (int i = 0; i < len; i++) iw[i + len] = complex(cos(pi * i / len), -sin(pi * i / len));
        for (int i = len - 1; i >= 0; i--) w[i] = w[i << 1], iw[i] = iw[i << 1];
        return lim;
    }
    inline void fft(complex *a) {
        for (int i = 0; i < lim; i++) if (i < rev[i]) std::swap(a[i], a[rev[i]]);
        for (int len = 1; len < lim; len <<= 1)
            for (int i = 0; i < lim; i += (len << 1))
                for (int j = 0; j < len; j++) {
                    complex x = a[i + j], y = a[i + j + len] * w[j + len];
                    a[i + j] = x + y, a[i + j + len] = x - y;
                }
    }
    inline void ifft(complex *a) {
        for (int i = 0; i < lim; i++) if (i < rev[i]) std::swap(a[i], a[rev[i]]);
        for (int len = 1; len < lim; len <<= 1)
            for (int i = 0; i < lim; i += (len << 1))
                for (int j = 0; j < len; j++) {
                    complex x = a[i + j], y = a[i + j + len] * iw[j + len];
                    a[i + j] = x + y, a[i + j + len] = x - y;
                }
        for (int i = 0; i < lim; i++) a[i].a /= lim;
    }
    inline poly operator * (const poly &F, const poly &G) {
        poly H(F.size() + G.size() - 1); lim = init(H.size());
        for (int i = 0; i < F.size(); i++) a[i] = F[i] >> 15, b[i] = F[i] & 32767;
        for (int i = 0; i < G.size(); i++) c[i] = G[i] >> 15, d[i] = G[i] & 32767;
        for (int i = F.size(); i < lim; i++) a[i] = b[i] = 0;
        for (int i = G.size(); i < lim; i++) c[i] = d[i] = 0;
        fft(a), fft(b), fft(c), fft(d);
        for (int i = 0; i < lim; i++)
            e[i] = a[i] * c[i], f[i] = a[i] * d[i], g[i] = b[i] * c[i], h[i] = b[i] * d[i];
        ifft(e), ifft(f), ifft(g), ifft(h);
        p30 %= mod, p15 %= mod;
        for (int i = 0; i < H.size(); i++)
            H[i] = ((ll)(e[i].a + 0.5) % mod * p30 % mod + (ll)(f[i].a + 0.5) % mod * p15 % mod + (ll)(g[i].a + 0.5) % mod * p15 % mod + (ll)(h[i].a + 0.5)) % mod;
        return H;
    }
}
using MTT::operator *;

void main() {
    read(n), read(k), read(L), read(x), read(y), read(mod), --x, --y;
    for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) read(A.a[i][j]);
    w[0] = 1, w[1] = fpow(get_root(mod), (mod - 1) / k);
    for (int i = 2; i < k; i++) w[i] = mul(w[i - 1], w[1]);
    I.a[0][0] = I.a[1][1] = I.a[2][2] = 1, S.a[0][x] = 1;
    for (int i = 0; i < k; i++) c[i] = (S * fpow(A * w[i] + I, L)).a[0][y];
    f.resize((k << 1) + 1), g.resize(k + 1);
    for (int i = 0; i < f.size(); i++) f[i] = get_omega(-((ll)i * (i - 1) >> 1));
    for (int i = 0; i < g.size(); i++) g[i] = mul(c[i], get_omega(((ll)i * (i - 1) >> 1)));
    // printf("w: "); for (int i = 0; i < k; i++) print(w[i], " \n"[i == k - 1]);
    // printf("c: "); for (int i = 0; i < k; i++) print(c[i], " \n"[i == k - 1]);
    // printf("f: "); for (int i = 0; i < f.size(); i++) print(f[i], " \n"[i == f.size() - 1]);
    // printf("g: "); for (int i = 0; i < g.size(); i++) print(g[i], " \n"[i == g.size() - 1]);
    std::reverse(g.begin(), g.end());
    f = f * g;	int inv_k = inv(k);
    for (int i = 0; i < k; i++) ans[i] = mul(f[k + i], mul(get_omega(((ll)i * (i - 1) >> 1)), inv_k));
    for (int i = 0; i < k; i++) print(ans[i], '\n');
}

} signed main() {
#ifdef MEMSET0_LOCAL_ENVIRONMENT
    freopen("1.in", "r", stdin);
#endif
    return ringo::main(), 0;
}