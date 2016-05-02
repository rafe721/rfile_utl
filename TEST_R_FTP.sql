
  CREATE OR REPLACE PACKAGE "VOICE_COMPRESSION"."TEST_R_FTP" AS
   PROCEDURE p_put_file_in_remote_system (
      p_msg_text        OUT      VARCHAR2
   );

   PROCEDURE p_get_file_from_remote_system (
      p_msg_text        OUT      VARCHAR2
   );
END TEST_R_FTP;
/
 

  CREATE OR REPLACE PACKAGE BODY "VOICE_COMPRESSION"."TEST_R_FTP" AS

  PROCEDURE p_put_file_in_remote_system (
      p_msg_text        OUT      VARCHAR2
   ) AS
   v_conn       rftp.connection;
   v_read_file  utl_tcp.connection;
  BEGIN

    v_conn := rftp.open('132.146.16.29', 'vcompr', 'compression', 'C:/', '/usr/users/PLAT/vcompr');
    
    rftp.open_file_to_write(v_conn, v_read_file, 'TEST_' || to_char(sysdate,'dddhh24mi'));
    
    rftp.put_line(v_read_file, 'Hi, Rahul is an awesome boy.');
    
    rftp.put_line(v_read_file, 'That is an undeniable fact.');
    
    rftp.close_file_connection(v_read_file);
    
    rftp.close(v_conn);
    
    p_msg_text := 'Successfully completed the transfer. Sent file to the remote system.';
  EXCEPTION
    WHEN OTHERS
    THEN
        p_msg_text := 'This sucks. Connection exception';
        rftp.close(v_conn);
        RAISE;
  END p_put_file_in_remote_system;

  PROCEDURE p_get_file_from_remote_system (
      p_msg_text        OUT      VARCHAR2
   ) AS
   v_conn         rftp.connection;
   v_read_file    utl_tcp.connection;
   v_data_string  VARCHAR2(1000);
  BEGIN

    v_conn := rftp.open('132.146.16.29', 'vcompr', 'compression', 'C:/', '/usr/users/PLAT/vcompr');
    
    rftp.open_file_to_read(v_conn, v_read_file, 'TEST_0561313');
    
    LOOP
        BEGIN
            rftp.get_line(v_read_file, v_data_string);
            DBMS_OUTPUT.put_line(v_data_string);
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                EXIT;
            WHEN OTHERS
            THEN
                RAISE;
                EXIT;
        END;
    END LOOP;
      
    rftp.close_file_connection(v_read_file);
    
    rftp.close(v_conn);
    
    p_msg_text := 'Successfully completed the transfer. Sent file to the remote system.';
  EXCEPTION
    WHEN OTHERS
    THEN
        p_msg_text := 'This sucks. Connection exception';
        rftp.close(v_conn);
        RAISE;
  END p_get_file_from_remote_system;

END TEST_R_FTP;
/
 
