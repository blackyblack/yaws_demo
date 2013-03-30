@echo off
erl -boot start_sasl -pa ./ebin -s stocks_app -kernel error_logger "{file, \"log/main.log\"}" ^
    -sasl errlog_type error -sasl sasl_error_logger "{file, \"log/sasl.log\"}"