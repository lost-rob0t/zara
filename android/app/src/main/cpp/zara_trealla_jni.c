#include <jni.h>
#include <pthread.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

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

static void append_detail_text(
    char *buffer,
    size_t capacity,
    size_t *length,
    const char *text)
{
    if (capacity == 0 || text == NULL || *length >= capacity)
        return;

    while (*text != '\0' && *length + 1 < capacity) {
        buffer[*length] = *text;
        (*length)++;
        text++;
    }
    buffer[*length] = '\0';
}

static void append_detail_size(
    char *buffer,
    size_t capacity,
    size_t *length,
    size_t value)
{
    if (capacity == 0 || *length >= capacity)
        return;

    char digits[sizeof(size_t) * 3];
    size_t digit_count = 0;
    do {
        digits[digit_count++] = (char)('0' + (value % 10));
        value /= 10;
    } while (value != 0 && digit_count < sizeof(digits));

    while (digit_count > 0 && *length + 1 < capacity) {
        buffer[*length] = digits[--digit_count];
        (*length)++;
    }
    buffer[*length] = '\0';
}

static void throw_query_state(
    JNIEnv *env,
    const char *message,
    bool query_ok,
    bool runtime_error,
    bool status,
    bool query_handle,
    size_t query_length)
{
    char detail[320] = {0};
    size_t detail_length = 0;
    append_detail_text(detail, sizeof(detail), &detail_length, message);
    append_detail_text(detail, sizeof(detail), &detail_length, " [pl_query_ok=");
    append_detail_text(detail, sizeof(detail), &detail_length, query_ok ? "true" : "false");
    append_detail_text(detail, sizeof(detail), &detail_length, " runtime_error=");
    append_detail_text(detail, sizeof(detail), &detail_length, runtime_error ? "true" : "false");
    append_detail_text(detail, sizeof(detail), &detail_length, " status=");
    append_detail_text(detail, sizeof(detail), &detail_length, status ? "true" : "false");
    append_detail_text(detail, sizeof(detail), &detail_length, " query_handle=");
    append_detail_text(detail, sizeof(detail), &detail_length, query_handle ? "true" : "false");
    append_detail_text(detail, sizeof(detail), &detail_length, " query_length=");
    append_detail_size(detail, sizeof(detail), &detail_length, query_length);
    append_detail_text(detail, sizeof(detail), &detail_length, "]");
    throw_state(env, detail);
}

static char *copy_result_text(const char *text, size_t length)
{
    if (text == NULL)
        return NULL;

    char *copy = malloc(length + 1);
    if (copy == NULL)
        return NULL;

    memcpy(copy, text, length);
    copy[length] = '\0';
    return copy;
}

static char *result_text(pl_term *term)
{
    if (pl_term_type(term) == PL_TYPE_STRING) {
        const char *text = pl_atom_text(term);
        if (text == NULL)
            return NULL;
        return copy_result_text(text, pl_atom_len(term));
    }

    char *canonical = pl_term_text(term);
    if (canonical == NULL)
        return NULL;

    char *copy = copy_result_text(canonical, strlen(canonical));
    pl_free(canonical);
    return copy;
}

static bool capture_result(pl_sub_query *query, char **results, size_t *count)
{
    pl_term *term = pl_binding(query, "Result");
    if (term == NULL)
        return false;

    char *text = result_text(term);
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

    const size_t query_length = strlen(query_source);
    char *results[ZARA_MAX_SEMANTIC_RESULTS] = {0};
    size_t count = 0;
    bool query_active = false;
    bool result_ok = true;

    pthread_mutex_lock(&g_runtime_lock);
    if (g_runtime == NULL) {
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, query_text, query_source);
        throw_state(env, "Trealla native runtime is not initialized");
        return NULL;
    }

    pl_sub_query *query = NULL;
    bool query_ok = pl_query(g_runtime, query_source, &query, 0);
    query_active = query != NULL;
    bool runtime_error = get_error(g_runtime);
    bool status = get_status(g_runtime);

    if (!query_ok || runtime_error) {
        bool query_handle = query_active;
        if (query_active) {
            pl_done(query);
            query_active = false;
        }
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, query_text, query_source);
        throw_query_state(
            env,
            "Trealla native query failed",
            query_ok,
            runtime_error,
            status,
            query_handle,
            query_length);
        return NULL;
    }

    if (status && !query_active) {
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, query_text, query_source);
        throw_query_state(
            env,
            "Trealla native query succeeded without an active query handle",
            query_ok,
            runtime_error,
            status,
            query_active,
            query_length);
        return NULL;
    }

    if (status) {
        if (!capture_result(query, results, &count)) {
            bool result_error = get_error(g_runtime);
            bool query_handle = query_active;
            if (query_active) {
                pl_done(query);
                query_active = false;
            }
            pthread_mutex_unlock(&g_runtime_lock);
            (*env)->ReleaseStringUTFChars(env, query_text, query_source);
            throw_query_state(
                env,
                "Trealla semantic query must bind Result",
                query_ok,
                result_error,
                status,
                query_handle,
                query_length);
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
                result_ok = false;
                break;
            }
        }
    } else if (query_active) {
        pl_done(query);
        query_active = false;
    }

    if (query_active) {
        pl_done(query);
        query_active = false;
    }

    runtime_error = get_error(g_runtime);
    pthread_mutex_unlock(&g_runtime_lock);
    (*env)->ReleaseStringUTFChars(env, query_text, query_source);

    if (!result_ok || runtime_error) {
        for (size_t i = 0; i < count; i++)
            free(results[i]);
        throw_query_state(
            env,
            "Trealla semantic result extraction failed",
            query_ok,
            runtime_error,
            status,
            query_active,
            query_length);
        return NULL;
    }

    jclass string_class = (*env)->FindClass(env, "java/lang/String");
    if (string_class == NULL) {
        for (size_t i = 0; i < count; i++)
            free(results[i]);
        return NULL;
    }

    jobjectArray output = (*env)->NewObjectArray(env, (jsize)count, string_class, NULL);
    if (output == NULL) {
        for (size_t i = 0; i < count; i++)
            free(results[i]);
        return NULL;
    }

    for (size_t i = 0; i < count; i++) {
        jstring value = (*env)->NewStringUTF(env, results[i]);
        free(results[i]);
        if (value == NULL)
            return NULL;
        (*env)->SetObjectArrayElement(env, output, (jsize)i, value);
        (*env)->DeleteLocalRef(env, value);
        if ((*env)->ExceptionCheck(env))
            return NULL;
    }

    return output;
}

JNIEXPORT jboolean JNICALL
Java_ai_zara_app_prolog_JniTreallaNativeApi_consult(
    JNIEnv *env,
    jobject self,
    jstring source_path)
{
    (void)self;
    if (source_path == NULL)
        return JNI_FALSE;

    const char *path = (*env)->GetStringUTFChars(env, source_path, NULL);
    if (path == NULL)
        return JNI_FALSE;

    pthread_mutex_lock(&g_runtime_lock);
    if (g_runtime == NULL) {
        pthread_mutex_unlock(&g_runtime_lock);
        (*env)->ReleaseStringUTFChars(env, source_path, path);
        return JNI_FALSE;
    }

    bool loaded = pl_consult(g_runtime, path);
    bool runtime_error = get_error(g_runtime);
    pthread_mutex_unlock(&g_runtime_lock);
    (*env)->ReleaseStringUTFChars(env, source_path, path);
    return loaded && !runtime_error ? JNI_TRUE : JNI_FALSE;
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
