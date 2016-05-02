
  CREATE OR REPLACE PACKAGE "VOICE_COMPRESSION"."TEST_REMOTE_FTP" AS
   PROCEDURE p_put_file_in_remote_system (
      p_msg_text        OUT      VARCHAR2
   );

   PROCEDURE p_get_file_from_remote_system (
      p_clob            OUT      CLOB,
      p_msg_text        OUT      VARCHAR2
   );
END TEST_REMOTE_FTP;
/
 

  CREATE OR REPLACE PACKAGE BODY "VOICE_COMPRESSION"."TEST_REMOTE_FTP" AS

  PROCEDURE p_put_file_in_remote_system (
      p_msg_text        OUT      VARCHAR2
   ) AS
   v_conn       sdsftp.connection;
   v_clob       CLOB;
  BEGIN
  
    DBMS_LOB.createtemporary(v_clob, TRUE);
    DBMS_LOB.open(v_clob, DBMS_LOB.lob_readwrite);
    
    DBMS_LOB.append(v_clob, 'Hi. Rahul is a great guy. That is an undeniable act.');
    --v_clob :='';
    sdsftp.clear_log;
    sdsftp.set_log_options(1);
    v_conn := sdsftp.open('132.146.16.29', 'vcompr', 'compression', 'C:/', '/usr/users/PLAT/vcompr');
    sdsftp.put_clob(v_conn, v_clob, 'TEST_' || to_char(sysdate,'dddhh24mi')  || '.txt');
    sdsftp.close(v_conn);

    IF DBMS_LOB.ISOPEN(v_clob) = 1
    THEN
        DBMS_LOB.close(v_clob);
    END IF;

    DBMS_LOB.freetemporary(v_clob);
    
    p_msg_text := 'Successfully completed the transfer. Sent file to the remote system.';
  EXCEPTION
    WHEN OTHERS
    THEN
        p_msg_text := 'This sucks. Connection exception';
        sdsftp.close(v_conn);
        RAISE;
  END p_put_file_in_remote_system;




  PROCEDURE p_get_file_from_remote_system (
      p_clob            OUT      CLOB,
      p_msg_text        OUT      VARCHAR2
   ) AS
   v_conn sdsftp.connection;
   v_clob      CLOB;
  BEGIN
    sdsftp.clear_log;
    sdsftp.set_log_options(0);
    v_conn := sdsftp.open('132.146.16.29', 'vcompr', 'compression', 'C:/', '/usr/users/PLAT/vcompr');
    DBMS_OUTPUT.
     put_line(
        '----------------------------------------------------------------------------'
    );
    v_clob := sdsftp.get_clob(v_conn, 'text1.txt');
    DBMS_OUTPUT.put_line('CLOB Size: ' || dbms_lob.getlength(v_clob));
    --DBMS_OUTPUT.put_line(sdsftp.get_clob(v_conn, 'TEST_3351957.txt'));
    DBMS_OUTPUT.
     put_line(
        '----------------------------------------------------------------------------'
    );
    sdsftp.close(v_conn);
        p_msg_text := 'Successfully completed the transfer. Got file from remote system.';
  EXCEPTION
    WHEN OTHERS
    THEN
        p_msg_text := 'This sucks. Connection exception';
        sdsftp.close(v_conn);
        RAISE;
  END p_get_file_from_remote_system;

END TEST_REMOTE_FTP;
/
 
