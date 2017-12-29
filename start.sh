#! /bin/bash
# 系统启动脚本
ROOT=`cd $(dirname $0); pwd`
# 日志目录
RUN_LOG_DIR=$ROOT/log

# 当前时间
DATETIME=`date "+%Y%m%d_%H%M%S"`

# error_logger相关配置
ERROR_LOG=${RUN_LOG_DIR}/error_${DATETIME}.log
SASL_LOG=${RUN_LOG_DIR}/sasl_${DATETIME}.log

# erlang cookie
COOKIE="private-cookie"

# 启动
erl \
    -setcookie ${COOKIE} \
    -pa ebin \
    -sname game_log_service \
    -boot start_sasl \
    -kernel error_logger \{file,\"${ERROR_LOG}\"\} \
    -sasl sasl_error_logger \{file,\"${SASL_LOG}\"\} \
    -s game_log_service 
