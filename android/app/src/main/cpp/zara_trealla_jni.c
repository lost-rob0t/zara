#include <jni.h>
#include <pthread.h>
#include <stdbool.h>
#include <stddef.h>

#include "trealla.h"

#define ZARA_MAX_SEMANTIC_RESULTS 256

static pthread_mutex_t g_runtime_lock = PTHREAD_MUTEX_INITIALIZER;
static prolog *g_runtime = NULL;

static void throw_state(JNIEnv *env, const char *message)
{
    jclass cls = (*env)->FindClass(env, "java/lang/IllegalStateException");
    if (cls != NULL)
        (*env)->ThrowNew(env, cls, message);
}

static bool capture_result(pl_sub_query *query, char **results, size_t *count)
{
    pl_term *term = pl_binding(query, "Result");
    if (term == NULL)
        return false;

    char *text = pl_term_text(term);
    if (text == NULL)
        return false;

    results[*count] = text;
    (*count)++;
    return true;
}

JNIEXPORT jboolean JNICALL
Java_ai_zara_app_prolog_JniTreallaNativeApi_initialize(
    JNIEnv *env,
    jobject self,
    jstring core_asset_path)
{
    (void)self;
    if (core_asset_path == NULL)
        return JNI_FALSE;

    const char *path = (*env)->GetStringUTFChars(env, core_asset_path, NULL);
    if (path == NULL)
        return JNI_FALSE;

    pthread_mutex_lock(&g_runtime_lock);
    if (g_runtime != NULL) {
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, core_asset_path, path);
        return JNI_FALSE;
    }

    prolog *runtime = pl_create();
    if (runtime == NULL || !pl_consult(runtime, path)) {
        if (runtime != NULL)
            pl_destroy(runtime);
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, core_asset_path, path);
        return JNI_FALSE;
    }

    set_quiet(runtime);
    set_dump_vars(runtime, 0);
    g_runtime = runtime;
    pthread_mutex_unlock(&g_runtime_lock);
    (*env)->ReleaseStringUTFChars(env, core_asset_path, path);
    return JNI_TRUE;
}

JNIEXPORT jobjectArray JNICALL
Java_ai_zara_app_prolog_JniTreallaNativeApi_evaluate(
    JNIEnv *env,
    jobject self,
    jstring query_text)
{
    (void)self;
    if (query_text == NULL) {
        throw_state(env, "Trealla query is required");
        return NULL;
    }

    const char *query_source = (*env)->GetStringUTFChars(env, query_text, NULL);
    if (query_source == NULL)
        return NULL;

    char *results[ZARA_MAX_SEMANTIC_RESULTS] = {0};
    size_t count = 0;
    bool query_active = false;
    bool ok = false;

    pthread_mutex_lock(&g_runtime_lock);
    if (g_runtime == NULL) {
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, query_text, query_source);
        throw_state(env, "Trealla native runtime is not initialized");
        return NULL;
    }

    pl_sub_query *query = NULL;
    ok = pl_query(g_runtime, query_source, &query, 0);
    query_active = query != NULL;

    if (!ok || get_error(g_runtime)) {
        if (query_active)
            pl_done(query);
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, query_text, query_source);
        throw_state(env, "Trealla native query failed");
        return NULL;
    }

    if (get_status(g_runtime)) {
        if (!capture_result(query, results, &count)) {
            if (query_active)
                pl_done(query);
            pthread_mutex_unlock(&g_runtime_lock);
            (*env)->ReleaseStringUTFChars(env, query_text, query_source);
            throw_state(env, "Trealla semantic query must bind Result");
            return NULL;
        }

        while (count < ZARA_MAX_SEMANTIC_RESULTS) {
            if (!pl_redo(query)) {
                query_active = false;
                break;
            }
            if (!capture_result(query, results, &count)) {
                pl_done(query);
                query_active = false;
                ok = false;
                break;
            }
        }
    } else if (query_active) {
        pl_done(query);
        query_active = false;
    }

    if (query_active)
        pl_done(query);

    bool query_error = get_error(g_runtime);
    pthread_mutex_unlock(&g_runtime_lock);
    (*env)->ReleaseStringUTFChars(env, query_text, query_source);

    if (!ok || query_error) {
        for (size_t i = 0; i < count; i++)
            pl_free(results[i]);
        throw_state(env, "Trealla semantic result extraction failed");
        return NULL;
    }

    jclass string_class = (*env)->FindClass(env, "java/lang/String");
    if (string_class == NULL) {
        for (size_t i = 0; i < count; i++)
            pl_free(results[i]);
        return NULL;
    }

    jobjectArray output = (*env)->NewObjectArray(env, (jsize)count, string_class, NULL);
    if (output == NULL) {
        for (size_t i = 0; i < count; i++)
            pl_free(results[i]);
        return NULL;
    }

    for (size_t i = 0; i < count; i++) {
        jstring value = (*env)->NewStringUTF(env, results[i]);
        pl_free(results[i]);
        if (value == NULL)
            return NULL;
        (*env)->SetObjectArrayElement(env, output, (jsize)i, value);
        (*env)->DeleteLocalRef(env, value);
        if ((*env)->ExceptionCheck(env))
            return NULL;
    }

    return output;
}

JNIEXPORT void JNICALL
Java_ai_zara_app_prolog_JniTreallaNativeApi_shutdown(
    JNIEnv *env,
    jobject self)
{
    (void)env;
    (void)self;

    pthread_mutex_lock(&g_runtime_lock);
    if (g_runtime != NULL) {
        pl_destroy(g_runtime);
        g_runtime = NULL;
    }
    pthread_mutex_unlock(&g_runtime_lock);
}
