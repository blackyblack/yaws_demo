{application,yaws,
 [{description,"yaws WWW server"},
  {vsn, "1.90"},
  {modules,[yaws, yaws_app, yaws_ticker, yaws_config, yaws_server, yaws_sup, yaws_api, yaws_log, yaws_logger, yaws_ls, yaws_debug, yaws_compile, yaws_ctl, yaws_cgi, yaws_zlib, yaws_generated, mime_types, yaws_session_server, yaws_outmod, yaws_revproxy, yaws_html, yaws_log_file_h, yaws_rss, yaws_dav, yaws_pam, json, json2, jsonrpc, yaws_jsonrpc, yaws_xmlrpc, haxe, yaws_rpc, yaws_soap_srv, yaws_soap_lib, authmod_gssapi, yaws_appmod_cgi, yaws_appmod_fcgi, yaws_sendfile, yaws_sup_restarts, yaws_stats, yaws_vdir, yaws_multipart, yaws_shaper, yaws_websockets]},
  {registered, []},
  {mod,{yaws_app,[]}},
  {env, [
         % {config, undefined},           % undefined | filename()
         % {debug, false},                % true | false
         % {trace, false},                % http | traffic | false
         % {traceoutput, false},          % true | false
         % {conf, "/etc/yaws.conf"},      % string()
         % {runmod, mymodule},            % atom()
         % {embedded, false},             % true | false
         % {id, "default"},               % string()
         % {pam_service, "system-auth"},  % string()
         % {pam_use_acct, true},          % true | false
         % {pam_use_sess, true}           % true | false
        ]},
  {applications,[kernel,stdlib]}]}.
