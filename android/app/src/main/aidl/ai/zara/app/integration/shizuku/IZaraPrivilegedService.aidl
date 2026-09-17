package ai.zara.app.integration.shizuku;

interface IZaraPrivilegedService {
    void destroy() = 16777114;
    int uid() = 1;
    String executeShell(String command, int timeoutMillis) = 2;
}
