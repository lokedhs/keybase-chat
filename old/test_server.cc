#include <iostream>
#include <sstream>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
    int server_socket = socket(AF_UNIX, SOCK_STREAM, 0);
    if(server_socket == -1) {
        perror("socket");
        return 1;
    }

    char *dir = getenv("XDG_RUNTIME_DIR");
    if(dir == NULL) {
        std::cerr << "XDG_RUNTIME_DIR not set\n";
        return 1;
    }

    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, "/tmp/test_server", sizeof(addr.sun_path));

    if(bind(server_socket, (struct sockaddr *)&addr, sizeof(addr)) == -1) {
        perror("bind");
        close(server_socket);
        return 1;
    }

    if(listen(server_socket, 2) == -1) {
        perror("listen");
        close(server_socket);
        return 1;
    }

    struct sockaddr_un caller_addr;
    socklen_t length;
    int socket = accept(server_socket, (sockaddr *)&caller_addr, &length);
    if(socket == -1) {
        perror("accept");
        close(server_socket);
        return 1;
    }

    std::cout << "connected\n";

    while(true) {
        unsigned char buf[10240];
        int n;
        if((n = read(socket, buf, sizeof(buf))) == -1) {
            perror("read");
            close(server_socket);
            return 1;
        }

        if(n == 0) {
            break;
        }

        std::cout << "got " << n << " bytes from socket\n";
        for(int i = 0 ; i < n ; i++) {
            int ch = buf[i];
            printf("buf[%d] = 0x%02x (%c)\n", i, ch, (ch >= 0x20 && ch <= 0x7e) ? ch : '.');
        }
    }

    close(server_socket);

    return 0;
}
