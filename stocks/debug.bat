@echo off
erl -boot start_sasl -pa ./ebin -s stocks_app -kernel error_logger "{file, \"log/main.log\"}" ^
    -sasl errlog_type all -sasl sasl_error_logger "{file, \"log/sasl.log\"}" ^
    -stocks debug_type debug