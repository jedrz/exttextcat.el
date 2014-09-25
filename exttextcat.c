#include <stdio.h>
#include <libexttextcat/textcat.h>

#define MAX_BUFFER_SIZE 10000
char buffer[MAX_BUFFER_SIZE + 1];

int read_from_file(const char* path)
{
    FILE *fp;
    int read_chars_len;

    if (!(fp = fopen(path, "r"))) {
        fprintf(stderr, "Input file does not exist");
        exit(1);
    }

    read_chars_len = fread(buffer, sizeof(char), MAX_BUFFER_SIZE, fp);
    if (read_chars_len == 0) {
        fprintf(stderr, "Error reading file\n");
    } else {
        buffer[++read_chars_len] = '\0';
    }
    return read_chars_len;
}

int read_from_stdin()
{
    char c;
    int cur_size = 0;

    while ((c = getchar()) != EOF && cur_size < MAX_BUFFER_SIZE) {
        buffer[cur_size++] = c;
    }
    buffer[cur_size] = '\0';
    return cur_size;
}

int main(int argc, char *argv[])
{
    void *textcat_handle;
    int read_chars_len;

    if (argc <= 1) {
        fprintf(stderr, "Usage: %s fpdb-configuration [input-file]\n", argv[0]);
        return 1;
    }

    if (argc == 2) {
        read_chars_len = read_from_stdin();
    } else {
        read_chars_len = read_from_file(argv[2]);
    }
    textcat_handle = textcat_Init(argv[1]);
    printf("%s\n", textcat_Classify(textcat_handle, buffer, read_chars_len));
    textcat_Done(textcat_handle);
    return 0;
}
