package ai.zara.prolog.ipc;

interface IPrologCallback {
    void onResult(String requestId, String resultJson);
    void onError(String requestId, String errorCode, String message);
}
