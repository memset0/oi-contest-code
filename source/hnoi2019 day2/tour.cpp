// =================================
//   author: memset0
//   date: 2019.04.10 18:43:43
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

const int N = 5e3 + 10;
char s[N];
int n, m, p;
int fa[N], vis[N];
bool flag, a[N], f[N][N];
std::vector<int> list, G[N];
std::queue<std::pair<int, int>> q;
std::vector<std::pair<int, int>> e;

inline void add_edge(int u, int v) {
    e.push_back(std::make_pair(u, v));
    if (!~u || !~v) return;
    G[u].push_back(v), G[v].push_back(u);
}

inline int find(int x) {
    if (fa[x] == x) return x;
    return fa[x] = find(fa[x]);
}

struct graph {
    std::string name;
    std::vector<int> G[N];
    std::vector<std::pair<int, int>> e;
    inline graph(const std::string& _name) { name = _name; }
    // inline std::vector<int>& operator [] (int k) { return G[k]; }
    inline void add_edge(int u, int v) {
        e.push_back(std::make_pair(u, v));
        G[u].push_back(v), G[v].push_back(u);
    }
    void dfs(int u) {
        list.push_back(u);
        for (auto v : G[u]) {
            // printf("%d[%d] -> %d[%d]\n", u, vis[u], v, vis[v]);
            if (~vis[v]) {
                if (vis[v] != (vis[u] ^ 1))
                    flag = true;
            } else {
                vis[v] = vis[u] ^ 1, dfs(v);
            }
        }
    }
    inline std::vector<std::pair<int, int>> solve() {
        // printf("===== solve %s =====\n", name.c_str());
        std::vector<std::pair<int, int>> ans;
        memset(vis, -1, sizeof(vis));
        for (int i = 1; i <= n; i++) fa[i] = i;
        for (const auto &it : e)
            if (find(it.first) != find(it.second)) {
                fa[find(it.first)] = find(it.second);
                ans.push_back(std::make_pair(it.first, it.second));
            }
        if (name == "G2") return ans;
        for (int i = 1; i <= n; i++)
            if (find(i) == i) {
                vis[i] = flag = 0, list.clear(), dfs(i);
                if (flag) for (auto &u : list)
                    ans.push_back(std::make_pair(u, u));
            }
        ans.push_back(std::make_pair(-1, -1));
        // for (int i = 1; i <= n; i++) print(find(i), " \n"[i == n]);
        // printf("===== =====\n");
        return ans;
    }
} G0("G0"), G1("G1"), G2("G2");

void main() {
    read(n), read(m), read(p), scanf("%s", s + 1);
    for (int i = 1; i <= n; i++) {
        a[i] = s[i] - '0';
        f[i][i] = 1, q.push(std::make_pair(i, i));
    }
    for (int i = 1, u, v; i <= m; i++) {
        read(u), read(v);
        if (!a[u] && !a[v]) G0.add_edge(u, v);
        if (a[u] && a[v]) G1.add_edge(u, v);
        if (a[u] ^ a[v]) G2.add_edge(u, v);
    }
    for (const auto &it : G0.solve()) add_edge(it.first, it.second);
    for (const auto &it : G1.solve()) add_edge(it.first, it.second);    
    for (const auto &it : G2.solve()) add_edge(it.first, it.second);
    for (const auto &it : e) if (a[it.first] == a[it.second]) {
        // printf("%d %d\n", it.first, it.second);
        if (!~it.first || !~it.second) continue;
        f[it.first][it.second] = 1, q.push(std::make_pair(it.first, it.second));
        f[it.second][it.first] = 1, q.push(std::make_pair(it.second, it.first));
    }
    while (q.size()) {
        int u = q.front().first, v = q.front().second; q.pop();
        for (auto _u : G[u]) for (auto _v : G[v]) if (a[_u] == a[_v]) {
            if (!f[_u][_v]) { f[_u][_v] = 1, q.push(std::make_pair(_u, _v)); }
        }
    }
    for (int i = 1, u, v, ans; i <= p; i++)
        read(u), read(v), puts(f[u][v] ? "YES" : "NO");
}

} signed main() {
#ifdef MEMSET0_LOCAL_ENVIRONMENT
    freopen("1.in", "r", stdin);
#endif
    return ringo::main(), 0;
}