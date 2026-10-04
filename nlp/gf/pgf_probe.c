/* Experimental process-isolated PGF probe, not an Android JNI singleton. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pgf/pgf.h>
#include <gu/string.h>
#include <gu/enum.h>

#define MAX_INPUT_BYTES 512
#define MAX_RESULT_BYTES 2048
#define MAX_CANDIDATES 8

static int emit_linearization(PgfConcr *language, PgfExpr expression,
                              GuPool *pool, GuExn *error, char *destination) {
    GuStringBuf *buffer = gu_new_string_buf(pool);
    pgf_linearize(language, expression, gu_string_buf_out(buffer), error);
    if (!gu_ok(error)) return 1;
    GuString text = gu_string_buf_freeze(buffer, pool);
    size_t length = strlen(text);
    if (length == 0 || length > MAX_RESULT_BYTES || strchr(text, '\n') != NULL) return 3;
    memcpy(destination, text, length + 1);
    return 0;
}

static int parse(PgfPGF *grammar, const char *text, GuPool *pool, GuExn *error) {
    PgfConcr *english = pgf_get_language(grammar, "ZaraEng");
    PgfConcr *canonical = pgf_get_language(grammar, "ZaraCanonical");
    if (english == NULL || canonical == NULL) return 1;
    PgfExprEnum *results = pgf_parse(english, pgf_start_cat(grammar, pool),
                                   text, error, pool, pool);
    if (!gu_ok(error)) return gu_exn_caught(error, PgfParseError) ? 2 : 1;
    if (results == NULL) return 2;
    char candidates[MAX_CANDIDATES][MAX_RESULT_BYTES + 1];
    size_t count = 0;
    for (; count <= MAX_CANDIDATES; ++count) {
        PgfExprProb *result = gu_next(results, PgfExprProb *, pool);
        if (!gu_ok(error)) return 1;
        if (result == NULL) break;
        if (count == MAX_CANDIDATES) return 3;
        int status = emit_linearization(canonical, result->expr, pool, error, candidates[count]);
        if (status != 0) return status;
    }
    if (count == 0) return 2;
    for (size_t index = 0; index < count; ++index) puts(candidates[index]);
    return 0;
}

static int render(PgfPGF *grammar, const char *tree, GuPool *pool, GuExn *error) {
    PgfConcr *english = pgf_get_language(grammar, "ZaraEng");
    if (english == NULL) return 1;
    PgfExpr expression = pgf_read_expr(gu_string_in(tree, pool), pool, pool, error);
    if (!gu_ok(error)) return 1;
    PgfType *type = pgf_read_type(gu_string_in("Reply", pool), pool, pool, error);
    if (!gu_ok(error) || type == NULL) return 1;
    pgf_check_expr(grammar, &expression, type, error, pool);
    if (!gu_ok(error)) return 1;
    char output[MAX_RESULT_BYTES + 1];
    int status = emit_linearization(english, expression, pool, error, output);
    if (status == 0) puts(output);
    return status;
}

int main(int argc, char **argv) {
    if (argc != 4) {
        fputs("usage: pgf-probe parse|render GRAMMAR INPUT\n", stderr);
        return 3;
    }
    if (strlen(argv[3]) > MAX_INPUT_BYTES) return 3;
    if (strcmp(argv[1], "parse") != 0 && strcmp(argv[1], "render") != 0) return 3;
    if (argv[3][0] == '\0') return 2;
    GuPool *pool = gu_new_pool();
    GuExn *error = gu_new_exn(pool);
    PgfPGF *grammar = pgf_read(argv[2], pool, error);
    int status = 1;
    if (gu_ok(error) && grammar != NULL) {
        status = strcmp(argv[1], "parse") == 0
            ? parse(grammar, argv[3], pool, error)
            : render(grammar, argv[3], pool, error);
    }
    if (status == 1) fputs("PGF runtime or grammar error\n", stderr);
    gu_pool_free(pool);
    return status;
}
