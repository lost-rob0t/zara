package ai.zara.prolog.ipc;

import ai.zara.prolog.ipc.IPrologCallback;

interface IPrologService {
    int getApiVersion();
    String getCapabilitiesJson();
    void query(String requestId, String goal, long deadlineEpochMs, IPrologCallback callback);
    void cancel(String requestId);
}
