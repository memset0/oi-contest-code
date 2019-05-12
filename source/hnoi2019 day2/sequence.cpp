// =================================
//   author: memset0
//   date: 2019.05.10 22:05:25
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
    x = 0; char c = getchar(); bool f = 0;
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
template <class T> inline void print(T a, int l, int r, std::string s = "") {
    if (s != "") std::cout << s << ": ";
    for (int i = l; i <= r; i++) print(a[i], " \n"[i == r]);
}

const int N = 2e5 + 10, mod = 998244353;
ll s3[N];
std::vector<std::pair<int, int>> qry[N];
int n, m, x, y, a[N], s1[N], s2[N], ans[N];

inline int sqr(int x) { return (ll)x * x - (ll)x * x / mod * mod; }
inline int dec(int a, int b) { a -= b; return a < 0 ? a + mod : a; }
inline int inc(int a, int b) { a += b; return a >= mod ? a - mod : a; }
inline int mul(int a, int b) { return (ll)a * b - (ll)a * b / mod * mod; }
inline int inv(int x) { return x < 2 ? 1 : mul(mod - mod / x, inv(mod % x)); }
inline int calc(int s0, int s1, int s2) { return dec(s2, mul(sqr(s1), inv(s0))); }

struct border { int block, val; };

struct frac {
    ll a; int b;
    inline frac() {}
    inline frac(ll x, int y) { a = x, b = y; }
    inline int v() const { return (a % mod * inv(b)) % mod; }
    inline bool operator<(const frac &other) const {
        return (__int128)a * other.b < (__int128)b * other.a;
    }
    inline bool operator<=(const frac &other) const {
        return (__int128)a * other.b <= (__int128)b * other.a;
    }
};

struct info {
    int l, r, cost; frac val;
    inline info() {}
    inline info(int _l, int _r) {
        l = _l, r = _r;
        val = frac(s3[r] - s3[l - 1], r - l + 1);
        cost = calc(r - l + 1, dec(s1[r], s1[l - 1]), dec(s2[r], s2[l - 1]));
    }
};

struct seg {
    info stk[N];
    int nod, flag, top, topBack[N];
    std::vector<std::pair<int, info>> mdf[N];
    struct node { int l, r, mid, sum; info x; } p[N << 2];

    inline seg(int f) { flag = f; }
    void build(int u, int l = 1, int r = n) {
        p[u].l = l, p[u].r = r, p[u].mid = (l + r) >> 1;
        if (l == r) return;
        build(u << 1, l, p[u].mid);
        build(u << 1 | 1, p[u].mid + 1, r);
    }
    void modify(int u, int k, const info &x) {
        if (p[u].l == p[u].r) { p[u].sum = (p[u].x = x).cost; return; }
        (k <= p[u].mid) ? modify(u << 1, k, x) : modify(u << 1 | 1, k, x);
        p[u].sum = inc(p[u << 1].sum, p[u << 1 | 1].sum);
    }
    info query(int u, int k) {
        if (p[u].l == p[u].r) return p[u].x;
        return (k <= p[u].mid) ? query(u << 1, k) : query(u << 1 | 1, k);
    }
    int querySum(int u, int l, int r) {
        if (p[u].l == l && p[u].r == r) return p[u].sum;
        if (r <= p[u].mid) return querySum(u << 1, l, r);
        if (l > p[u].mid) return querySum(u << 1 | 1, l, r);
        return inc(querySum(u << 1, l, p[u].mid), querySum(u << 1 | 1, p[u].mid + 1, r));
    }
    
    void add(int k) {
        info now(k, k);
        stk[++top] = now;
        if (flag == 1) mdf[k].push_back(std::make_pair(top, now));
        while (top >= 2) {
            info pre = stk[top - 1];
            if (flag == 1) {
                if (now.val < pre.val) {
                    stk[--top] = now = info(pre.l, now.r);
                    mdf[k].push_back(std::make_pair(top, now));
                } else break;
            } else {
                if (pre.val < now.val) {
                    mdf[k].push_back(std::make_pair(top - 1, stk[top - 1]));
                    stk[--top] = now = info(now.l, pre.r);
                } else break;
            }
        }
        topBack[k] = top;
        // printf("[%d] (%d) => ", flag, k);
        // for (int i = 1; i <= top; i++)
        // 	printf("[%d %d : %d]%c", stk[i].l, stk[i].r, stk[i].cost, " \n"[i == top]);
    }
    
    void play(int k) {
        if (flag == 1) {
            top = topBack[k - 1];
            for (auto it = mdf[k - 1].begin(); it != mdf[k - 1].end(); it++)
                if (it->first <= top) {
                    stk[it->first] = it->second;
                    modify(1, it->first, it->second);
                }
        } else {
            top = topBack[k + 1];
            for (auto it = mdf[k].rbegin(); it != mdf[k].rend(); it++)
                if (it->first <= top) {
                    stk[it->first] = it->second;
                    modify(1, it->first, it->second);
                }
        }
        // printf("[%d] (%d) => ", flag, k);
        // for (int i = 1; i <= top; i++)
        // 	printf("[%d %d : %d]%c", stk[i].l, stk[i].r, stk[i].cost, ' ');
        // putchar('\n');
    }
} t1(1), t2(2);

inline bool checkL(const border &l, const border &r) {
    // printf("> check left [%d %d] [%d %d]\n", l.val, l.block, r.val, r.block);
    frac mid(s3[r.val] - s3[l.val - 1] + y - a[x], r.val - l.val + 1);
    return (l.block && mid <= t1.stk[l.block].val) ^ 1;
}

inline bool checkR(const border &l, const border &r) {
    // printf("> check right [%d %d] [%d %d]\n", l.val, l.block, r.val, r.block);
    frac mid(s3[r.val] - s3[l.val - 1] + y - a[x], r.val - l.val + 1);
    return (r.block && t2.stk[r.block].val <= mid) ^ 1;
}

border searchL(const border &R) {
    int l = 0, r = t1.top; border ans = {-1, -1};
    while (l <= r) {
        int mid = (l + r) >> 1;
        border L = {mid, mid == t1.top ? x : t1.stk[mid + 1].l};
        checkL(L, R) ? (ans = L, l = mid + 1) : r = mid - 1;
    }
    return ans;
}

std::pair<border, border> searchR() {
    int l = 0, r = t2.top; std::pair<border, border> ans({-1, -1}, {-1, -1});
    while (l <= r) {
        int mid = (l + r) >> 1;
        border R = {mid, mid == t2.top ? x : t2.stk[mid + 1].r};
        border L = searchL(R);
        checkR(L, R) ? (ans = std::make_pair(L, R), l = mid + 1) : r = mid - 1;
    } return ans;
}

int calc(const std::pair<border, border> &it) {
    // printf("calc [%d %d] [%d %d]\n", it.first.val, it.first.block, it.second.val, it.second.block);
    int l = it.first.val, r = it.second.val;
    int res = calc(r - l + 1, inc(dec(s1[r], s1[l - 1]), dec(y, a[x])), inc(dec(s2[r], s2[l - 1]), dec(sqr(y), sqr(a[x]))));
    if (it.first.block) res = inc(res, t1.querySum(1, 1, it.first.block));
    if (it.second.block) res = inc(res, t2.querySum(1, 1, it.second.block));
    return res;
}

void main() {
    read(n), read(m);
    for (int i = 1; i <= n; i++) {
        read(a[i]);
        s3[i] = s3[i - 1] + a[i];
        s1[i] = inc(s1[i - 1], a[i]);
        s2[i] = inc(s2[i - 1], sqr(a[i]));
    }
    t1.build(1, 1, n), t2.build(1, 1, n);
    for (int i = 1; i <= n; i++) t1.add(i);
    for (int i = n; i >= 1; i--) t2.add(i);
    for (int i = 1; i <= t1.top; i++) ans[0] = inc(ans[0], t1.stk[i].cost);
    for (int i = 1; i <= t2.top; i++) t2.modify(1, i, t2.stk[i]);
    for (int x, y, i = 1; i <= m; i++) {
        read(x), read(y);
        qry[x].push_back(std::make_pair(y, i));
    }
    for (int i = 1; i <= n; i++) {
        t1.play(i), t2.play(i), x = i;
        // printf("t1.top = %d | t2.top = %d\n", t1.top, t2.top);
        // for (int i = 1; i <= t1.top; i++) printf("[%d %d]%c", t1.stk[i].l, t1.stk[i].r, " \n"[i == t1.top]);
        // for (int i = 1; i <= t1.top; i++) printf("[%d %d]%c", t1.query(1, i).l, t1.query(1, i).r, " \n"[i == t1.top]);
        // for (int i = 1; i <= t2.top; i++) printf("[%d %d]%c", t2.stk[i].l, t2.stk[i].r, " \n"[i == t2.top]);
        // for (int i = 1; i <= t2.top; i++) printf("[%d %d]%c", t2.query(1, i).l, t2.query(1, i).r, " \n"[i == t2.top]);
        for (const auto &it : qry[i]) {
            // printf(">> %d %d %d\n", i, it.first, it.second);
            y = it.first;
            ans[it.second] = calc(searchR());
            // printf("<< %d\n", ans[it.second]);
        }
    }
    for (int i = 0; i <= m; i++) print(ans[i], '\n');
}

} signed main() {
#ifdef MEMSET0_LOCAL_ENVIRONMENT
    freopen("1.in", "r", stdin);
#endif
    return ringo::main(), 0;
}