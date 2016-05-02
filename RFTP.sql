
  CREATE OR REPLACE PACKAGE "VOICE_COMPRESSION"."RFTP" AS

  TYPE connection IS RECORD
    (
        tcp               UTL_TCP.connection,
        account_info      VARCHAR2(1000),
        transfer_method   VARCHAR2(1),
        transfer_option   VARCHAR2(1),
        local_directory   VARCHAR2(30),
        last_reply        VARCHAR2(32767)
    );
    
    -- Default FTP control Port.
    c_default_ftp_control_port   CONSTANT INTEGER := 21;
    
    -- Default Transfer method.
    c_default_transfer_method    CONSTANT VARCHAR2(10) := 'ASCII';

    -- Default Verbose.
    c_default_verbose            CONSTANT BOOLEAN := FALSE;
    
    -- To Open a new connection.
    PROCEDURE open(p_server             IN     VARCHAR2,
                   p_username           IN     VARCHAR2,
                   p_password           IN     VARCHAR2,
                   p_connection         OUT    connection,
                   p_local_directory    IN     VARCHAR2 DEFAULT NULL,
                   p_remote_directory   IN     VARCHAR2 DEFAULT NULL,
                   p_trans_method       IN     VARCHAR2 DEFAULT c_default_transfer_method,
                   p_timeout            IN     INTEGER DEFAULT NULL,
                   p_port               IN     INTEGER DEFAULT c_default_ftp_control_port,
                   p_account_info       IN     VARCHAR2 DEFAULT NULL
                  );
                  
    FUNCTION open(p_server             IN VARCHAR2,
                  p_username           IN VARCHAR2,
                  p_password           IN VARCHAR2,
                  p_local_directory    IN VARCHAR2 DEFAULT NULL,
                  p_remote_directory   IN VARCHAR2 DEFAULT NULL,
                  p_trans_method       IN VARCHAR2 DEFAULT c_default_transfer_method,
                  p_timeout            IN INTEGER DEFAULT NULL,
                  p_port               IN INTEGER DEFAULT c_default_ftp_control_port,
                  p_account_info       IN VARCHAR2 DEFAULT NULL
                 )
        RETURN connection;
                  
    -- to change directories in the remote server.
    PROCEDURE remote_cd(p_connection IN OUT NOCOPY connection, p_directory IN VARCHAR2);
    
    PROCEDURE local_cd(p_connection IN OUT NOCOPY connection, p_directory IN VARCHAR2);
                  
    -- To Close an open connection.
    PROCEDURE close(p_connection IN OUT NOCOPY connection);
    
    -- To set the transfer method.
    PROCEDURE set_transfer_method(p_connection        IN OUT NOCOPY connection,
                                  p_transfer_method   IN            VARCHAR2,
                                  p_option            IN            VARCHAR2 DEFAULT NULL
                                 );

    -- To get the present Transfer method.
    FUNCTION get_transfer_method(p_connection IN OUT NOCOPY connection)
        RETURN VARCHAR2;
    
    -- To send FTP control commands to the remote system.
    PROCEDURE send_ftp_command(p_connection     IN OUT NOCOPY connection,
                               p_command        IN            VARCHAR2,
                               p_arguments      IN            VARCHAR2 DEFAULT NULL,
                               p_account_info   IN            VARCHAR2 DEFAULT NULL
                              );
                              
    -- To create a passive connection to a particular file.
    PROCEDURE open_file_To_Read(p_connection        IN OUT        connection,
                                   p_file_connection   OUT           UTL_TCP.connection,
                                   p_remote_file_name  IN            VARCHAR2,
                                   p_transfer_method   IN            VARCHAR2 DEFAULT NULL,
                                   p_option            IN            VARCHAR2 DEFAULT NULL
                                 );

    -- To create a passive connection to a particular file.
    PROCEDURE open_file_To_write(p_connection        IN OUT        connection,
                                   p_file_connection   OUT           UTL_TCP.connection,
                                   p_remote_file_name  IN            VARCHAR2,
                                   p_transfer_method   IN            VARCHAR2 DEFAULT NULL,
                                   p_option            IN            VARCHAR2 DEFAULT NULL
                                 );
                                 
    -- To create a passive connection to a particular file.
    PROCEDURE close_file_connection(p_file_connection   IN OUT           UTL_TCP.connection
                                   );

    -- To write a line into the file.
    PROCEDURE put_line(p_connection        IN OUT        UTL_TCP.connection,
                       p_line_data         IN           VARCHAR2
                       );
    
    -- To read a line from the file.
    PROCEDURE get_line(p_connection        IN OUT           UTL_TCP.connection,
                       p_line_data         IN OUT           VARCHAR2
                       );
    
    PROCEDURE read_reply(p_connection IN OUT NOCOPY connection);
    
    FUNCTION reply_code(p_reply_text IN VARCHAR2)
        RETURN INTEGER;

END RFTP;
/
 

  CREATE OR REPLACE PACKAGE BODY "VOICE_COMPRESSION"."RFTP" AS

  c_ftp_request_acct        CONSTANT INTEGER := 332;


  PROCEDURE open(p_server             IN     VARCHAR2,
                   p_username           IN     VARCHAR2,
                   p_password           IN     VARCHAR2,
                   p_connection         OUT    connection,
                   p_local_directory    IN     VARCHAR2 DEFAULT NULL,
                   p_remote_directory   IN     VARCHAR2 DEFAULT NULL,
                   p_trans_method       IN     VARCHAR2 DEFAULT c_default_transfer_method,
                   p_timeout            IN     INTEGER DEFAULT NULL,
                   p_port               IN     INTEGER DEFAULT c_default_ftp_control_port,
                   p_account_info       IN     VARCHAR2 DEFAULT NULL
                  ) AS
        v_connection   connection;
  BEGIN
        v_connection.tcp      :=
            UTL_TCP.open_connection(remote_host  => p_server,
                                    remote_port  => p_port,
                                    tx_timeout   => p_timeout
                                   );

        read_reply(v_connection);

        send_ftp_command(v_connection, 'USER', p_username, p_account_info);
        send_ftp_command(v_connection, 'PASS', p_password, p_account_info);

        -- If we haven't already been prompted for ACCT info
        -- then send it now if we have it
        IF p_account_info IS NOT NULL AND v_connection.account_info IS NULL
        THEN
            send_ftp_command(v_connection, 'ACCT', p_account_info);
            v_connection.account_info  := p_account_info;
        END IF;

        IF p_local_directory IS NOT NULL
        THEN
            local_cd(v_connection, p_local_directory);
        END IF;

        IF p_remote_directory IS NOT NULL
        THEN
            remote_cd(v_connection, p_remote_directory);
        END IF;

        set_transfer_method(v_connection, p_trans_method);

        p_connection  := v_connection;
  END open;
  
  FUNCTION open(p_server             IN VARCHAR2,
                  p_username           IN VARCHAR2,
                  p_password           IN VARCHAR2,
                  p_local_directory    IN VARCHAR2 DEFAULT NULL,
                  p_remote_directory   IN VARCHAR2 DEFAULT NULL,
                  p_trans_method       IN VARCHAR2 DEFAULT c_default_transfer_method,
                  p_timeout            IN INTEGER DEFAULT NULL,
                  p_port               IN INTEGER DEFAULT c_default_ftp_control_port,
                  p_account_info       IN VARCHAR2 DEFAULT NULL
                 )
        RETURN connection
    IS
        v_connection   connection;
    BEGIN
        rftp.open(p_server            => p_server,
                    p_username          => p_username,
                    p_password          => p_password,
                    p_connection        => v_connection,
                    p_local_directory   => p_local_directory,
                    p_remote_directory  => p_remote_directory,
                    p_trans_method      => p_trans_method,
                    p_timeout           => p_timeout,
                    p_port              => p_port,
                    p_account_info      => p_account_info
                   );
        RETURN v_connection;
    END open;

  PROCEDURE remote_cd(p_connection IN OUT NOCOPY connection, p_directory IN VARCHAR2) AS
  BEGIN
    send_ftp_command(p_connection, 'CWD', p_directory);
  END remote_cd;
  
    -- Change the directory used locally for sending files from or retrieving files into
    -- a directory can be an explicit path supported by utl_file_dir parameter
    -- or it can be a directory object.
    PROCEDURE local_cd(p_connection IN OUT NOCOPY connection, p_directory IN VARCHAR2)
    IS
    BEGIN
        p_connection.local_directory  := p_directory;
    END local_cd;

  PROCEDURE close(p_connection IN OUT NOCOPY connection) AS
  BEGIN
        send_ftp_command(p_connection, 'QUIT');
        UTL_TCP.close_connection(p_connection.tcp);
        p_connection.tcp              := NULL;
        p_connection.account_info     := NULL;
        p_connection.transfer_method  := NULL;
        p_connection.transfer_option  := NULL;
        p_connection.local_directory  := NULL;
        p_connection.last_reply       := NULL;
  END close;

  PROCEDURE set_transfer_method(p_connection        IN OUT NOCOPY connection,
                                  p_transfer_method   IN            VARCHAR2,
                                  p_option            IN            VARCHAR2 DEFAULT NULL
                                 ) AS
        v_method       VARCHAR2(10) := UPPER(SUBSTR(p_transfer_method, 1, 10));
        v_option       VARCHAR2(1) := UPPER(SUBSTR(p_option, 1, 1));

        v_new_method   VARCHAR2(3);
  BEGIN
        CASE
            WHEN v_method IS NULL
            THEN
                -- Do nothing, if NULL is the new method, then just keep the current one
                NULL;
            WHEN v_method = 'DEFAULT'
            THEN
                v_new_method  := SUBSTR(c_default_transfer_method, 1, 1);
            WHEN v_method = 'BINARY'
            THEN
                v_new_method  := 'I';
            WHEN v_method IN ('A', 'ASCII', 'I', 'IMAGE', 'E', 'EBCDIC')
            THEN
                v_new_method  := SUBSTR(v_method, 1, 1);
            ELSE
                raise_application_error(
                    -20001,
                    'SDSFTP.set_transfer_method INVALID method: ' || p_transfer_method,
                    TRUE);
        END CASE;

        IF v_new_method != NVL(p_connection.transfer_method, '---')
        THEN

            IF v_option IN ('N', 'T', 'C')
            THEN
                send_ftp_command(p_connection, 'TYPE', v_new_method || ' ' || v_option);
            ELSIF v_option IS NULL
            THEN
                send_ftp_command(p_connection, 'TYPE', v_new_method);
            ELSE
                raise_application_error(-20001,
                                        'SDSFTP.set_transfer_method INVALID option: ' || p_option,
                                        TRUE
                                       );
            END IF;

            p_connection.transfer_method  := v_new_method;
            p_connection.transfer_option  := v_option;
        END IF;
  END set_transfer_method;

  FUNCTION get_transfer_method(p_connection IN OUT NOCOPY connection)
        RETURN VARCHAR2 AS
  BEGIN
        RETURN CASE
                   WHEN p_connection.transfer_method = 'A' THEN 'ASCII'
                   WHEN p_connection.transfer_method = 'E' THEN 'EBCDIC'
                   WHEN p_connection.transfer_method = 'I' THEN 'IMAGE'
               END;
  END get_transfer_method;
  
  -- Request a separate connection on a different port for data transfer
    -- PASV is the preferred transfer method opposed to PORT
    -- per RFC 1123 (4.1.2.6) all servers must implement PASV.
    FUNCTION get_passive_connection(p_connection IN OUT NOCOPY connection)
        RETURN UTL_TCP.connection
    IS
        v_temp              VARCHAR2(25);
        v_host              VARCHAR2(25);
        v_port              INTEGER;
        v_pasv_connection   UTL_TCP.connection;
    BEGIN
        send_ftp_command(p_connection, 'PASV');

        -- PASV should respond with something of the form 227 zzzzzzzzzzzzzz (hhh,hhh,hhh,hhh,ppp,ppp)
        -- where  hhh,hhh,hhh,hhh  is the host ip address, simply change ','  to '.' and it's ready to go
        -- construct the port by taking the first part as the high byte of a 2-byte number  (multiply by 256)
        -- and the second part as the low byte of the 2-byte number  (add it to the high byte)

        -- per RFC1123 the host/port digits might not be enclosed in parentheses(),
        -- therefore the parsing should be based on a scan of the digits themselves

        v_temp             := REGEXP_SUBSTR(p_connection.last_reply, '(\d{1,3},){5,5}\d{1,3}');

        --
        -- 9i and lower don't have regular expressions
        -- so, we have to use other methods to strip out the host/port digits
        --        v_temp :=
        --            RTRIM(
        --                LTRIM(
        --                    TRANSLATE(
        --                        SUBSTR(p_connection.last_reply, 5),
        --                        '0123456789,'
        --                        || TRANSLATE(SUBSTR(p_connection.last_reply, 5), CHR(0) || '0123456789,', CHR(0)),
        --                        '0123456789,'
        --                    ),
        --                    ','
        --                ),
        --                ','
        --            );

        --  v_temp should now look like this: 'hhh,hhh,hhh,hhh,ppp,ppp'  (minus the quotes)

        v_host             := REPLACE(SUBSTR(v_temp, 1, INSTR(v_temp, ',', 1, 4) - 1), ',', '.');

        v_temp             := SUBSTR(v_temp, INSTR(v_temp, ',', 1, 4) + 1);

        v_port             :=
            TO_NUMBER(SUBSTR(v_temp, 1, INSTR(v_temp, ',') - 1)) * 256
            + TO_NUMBER(SUBSTR(v_temp, INSTR(v_temp, ',') + 1));

        v_pasv_connection  := UTL_TCP.open_connection(remote_host => v_host, remote_port => v_port);

        RETURN v_pasv_connection;
    EXCEPTION
        WHEN OTHERS
        THEN
            RAISE;
    END get_passive_connection;

  PROCEDURE send_ftp_command(p_connection     IN OUT NOCOPY connection,
                               p_command        IN            VARCHAR2,
                               p_arguments      IN            VARCHAR2 DEFAULT NULL,
                               p_account_info   IN            VARCHAR2 DEFAULT NULL
                              ) AS
    v_code   INTEGER;
  BEGIN
    IF p_arguments IS NULL
        THEN
            v_code  := UTL_TCP.write_line(p_connection.tcp, p_command);
        ELSE
            v_code  := UTL_TCP.write_line(p_connection.tcp, p_command || ' ' || p_arguments);
        END IF;

        read_reply(p_connection);

        IF reply_code(p_connection.last_reply) = c_ftp_request_acct
        THEN
            send_ftp_command(p_connection, 'ACCT', NVL(p_connection.account_info, p_account_info));
            p_connection.account_info  := p_account_info;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            RAISE;
  END send_ftp_command;

  PROCEDURE open_file_To_Read(p_connection        IN OUT        connection,
                                   p_file_connection   OUT           UTL_TCP.connection,
                                   p_remote_file_name  IN            VARCHAR2,
                                   p_transfer_method   IN            VARCHAR2 DEFAULT NULL,
                                   p_option            IN            VARCHAR2 DEFAULT NULL
                                 ) AS
  BEGIN
    set_transfer_method(p_connection, p_transfer_method);
    p_file_connection        := get_passive_connection(p_connection);

    send_ftp_command(p_connection, 'RETR', p_remote_file_name);
  END open_file_To_Read;
  
  PROCEDURE open_file_To_write(p_connection        IN OUT           connection,
                                   p_file_connection   OUT           UTL_TCP.connection,
                                   p_remote_file_name  IN            VARCHAR2,
                                   p_transfer_method   IN            VARCHAR2 DEFAULT NULL,
                                   p_option            IN            VARCHAR2 DEFAULT NULL
                                 ) AS
  BEGIN
    set_transfer_method(p_connection, p_transfer_method);
    p_file_connection        := get_passive_connection(p_connection);

    send_ftp_command(p_connection, 'STOR', p_remote_file_name);
  END open_file_To_Write;

  PROCEDURE close_file_connection(p_file_connection   IN OUT           UTL_TCP.connection
                                   ) AS
  BEGIN
    UTL_TCP.close_connection(p_file_connection);
  END close_file_connection;

  PROCEDURE put_line(p_connection        IN OUT         UTL_TCP.connection,
                       p_line_data         IN           VARCHAR2
                       ) AS
        v_write_cnt    INTEGER;
  BEGIN
    v_write_cnt := utl_tcp.write_line(p_connection, p_line_data);
    --UTL_TCP.write_text(p_connection, p_line_data, NULL);
    UTL_TCP.flush(p_connection);
  END put_line;

  PROCEDURE get_line(p_connection        IN OUT           UTL_TCP.connection,
                       p_line_data       IN OUT           VARCHAR2
                       ) AS
        v_read_cnt    INTEGER;
  BEGIN
    BEGIN
      v_read_cnt := utl_tcp.read_line(p_connection, p_line_data, TRUE);
    EXCEPTION
                WHEN UTL_TCP.end_of_input
                THEN
                    RAISE NO_DATA_FOUND;
                WHEN OTHERS
                THEN
                    RAISE;
            END;
  END get_line;
  
  -- Read a reply from the server, including multi-line replies, concatenating them into a single reply string
    PROCEDURE read_reply(p_connection IN OUT NOCOPY connection)
    IS
        --    FTP Replies (per rfc959)
        --        replies begin with 3 digit codes xyz
        --        the from can be either single line or multi-line
        --        if single line, then xyz text
        --        if multi-line, then xyz-text, followed by any number of lines, followed by xyz text
        --        thus a reply will always end with the xyz code, a space and optionally some text.
        --
        --        xyz single line reply
        --
        --        xyz-start of multi-line reply
        --            some text
        --            some more text
        --        xyz end of multi-line reply
        --
        --        1yz   Positive Preliminary reply
        --        2yz   Positive Completion reply
        --        3yz   Positive Intermediate reply
        --        4yz   Transient Negative Completion reply
        --        5yz   Permanent Negative Completion reply
        --
        --        x0z   Syntax
        --        x1z   Information
        --        x2z   Connections
        --        x3z   Authentication and accounting
        --        x4z   Unspecified as yet.
        --        x5z   File system
        --
        --        The third digit gives a finer gradation of meaning in each
        --        of the function categories, specified by the second digit.
        v_reply        VARCHAR2(32767) := NULL;
        v_start_code   VARCHAR2(3);
        v_temp         VARCHAR2(32767);
    BEGIN
        v_temp                   := UTL_TCP.get_line(p_connection.tcp, TRUE);

        IF SUBSTR(v_temp, 4, 1) = ' '
        THEN
            -- 3 digits and a space is a normal, one line response
            v_reply  := v_temp;
        ELSIF SUBSTR(v_temp, 4, 1) = '-'
        THEN
            v_start_code  := SUBSTR(v_temp, 1, 3);

            LOOP
                v_temp  := UTL_TCP.get_line(p_connection.tcp, TRUE);

                IF v_reply IS NULL
                THEN
                    v_reply  := v_temp;
                ELSE
                    v_reply  := v_reply || UTL_TCP.crlf || v_temp;
                END IF;

                EXIT WHEN v_temp LIKE v_start_code || ' %';
            END LOOP;
        ELSE
            raise_application_error(-20001, 'Invalid FTP Protocol reply: ' || v_temp, TRUE);
        END IF;

        p_connection.last_reply  := v_reply;

        IF SUBSTR(v_reply, 1, 1) = '4'
        THEN
            raise_application_error(-20001, 'Transient error from FTP server: ' || v_reply, TRUE);
        ELSIF SUBSTR(v_reply, 1, 1) = '5'
        THEN
            raise_application_error(-20001, 'Permanent error from FTP server: ' || v_reply, TRUE);
        END IF;
    EXCEPTION
        WHEN UTL_TCP.end_of_input
        THEN
            NULL;
    END read_reply;
    
    -- To get Reply code.
    FUNCTION reply_code(p_reply_text IN VARCHAR2)
        RETURN INTEGER
    IS
    BEGIN
        RETURN TO_NUMBER(SUBSTR(p_reply_text, 1, 3));
    END reply_code;

END RFTP;
/
 
