#!/bin/sh
# Tailor mbedtls_config.h for proper threading support

# First argument must be either 'pthreads' or 'win32'
thread_model=$1

# Second argument must be the path of mbedtls_config.h
config_path=$2

if [ -z "$thread_model" -o -z "$config_path" ]; then
    echo "Usage: process-config.sh $GAUCHE_THREAD_TYPE $PATH_TO_MBEDTLS_CONFIG_H"
    exit 1
fi

case $thread_model in
    pthreads)
        sed -e 's@^//#define MBEDTLS_PLATFORM_MEMORY@#define MBEDTLS_PLATFORM_MEMORY@' \
            -e 's@^//#define MBEDTLS_THREADING_C@#define MBEDTLS_THREADING_C@' \
            -e 's@^//#define MBEDTLS_THREADING_PTHREAD@#define MBEDTLS_THREADING_PTHREAD@' \
            -e 's@^#define MBEDTLS_THREADING_ALT@//#define MBEDTLS_THREADING_ALT@' \
            $config_path > ${config_path}.tmp \
          && mv ${config_path}.tmp $config_path
        ;;
    win32)
        # We need to prepare threading_alt.h before doing this
        # sed -e 's@^//#define MBEDTLS_PLATFORM_MEMORY@#define MBEDTLS_PLATFORM_MEMORY@' \
        #     -e 's@^//#define MBEDTLS_THREADING_C@#define MBEDTLS_THREADING_C@' \
        #     -e 's@^#define MBEDTLS_THREADING_PTHREAD@//#define MBEDTLS_THREADING_PTHREAD@' \
        #     -e 's@^//#define MBEDTLS_THREADING_ALT@#define MBEDTLS_THREADING_ALT@' \
        #     $config_path > ${config_path}.tmp \
        #   && mv ${config_path}.tmp $config_path
        ;;
    *)
        echo "process-config.sh: Uknown thread model: $thread_model"
        exit 1
        ;;
esac
