FROM clfoundation/sbcl:2.2.4

ENV QUICKLISP_ADD_TO_INIT_FILE=true
ENV QUICKLISP_DIST_VERSION=latest

WORKDIR /app
COPY . .

RUN /usr/local/bin/install-quicklisp

ENTRYPOINT ["./entrypoint.sh"]
