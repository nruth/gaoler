-define(LOGFILE, "logfile.log").
-define(LOGDIRECTORY, "acceptor_logs").
-define(LOG(Election), ?LOGDIRECTORY++
            "/"++integer_to_list(Election)++"_"
            ++atom_to_list(node())++".log").
