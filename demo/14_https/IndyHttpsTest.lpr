program IndyHttpsTest;

{$APPTYPE CONSOLE}

uses
  IdSSLOpenSSL,
  IdGlobal, IdHTTPServer,
  SysUtils, Classes, ShellAPI;

procedure Start;
var
  IOHandler: TIdServerIOHandlerSSLOpenSSL;
  Server: TIdHTTPServer;
begin
  // OpenSSL Handler erzeugen
  IOHandler := TIdServerIOHandlerSSLOpenSSL.Create;
  IOHandler.SSLOptions.CertFile := 'cert.pem';
  IOHandler.SSLOptions.KeyFile := 'key.pem';
  IOHandler.SSLOptions.RootCertFile := 'cacert.pem';
  IOHandler.SSLOptions.Mode := sslmServer;

  Server := TIdHTTPServer.Create;
  try
    Server.DefaultPort := 443;
    Server.IOHandler := IOHandler;
    Server.Active := True;

    // start the server
    WriteLn(Format('Server is listening on port %d', [Server.DefaultPort]));

    // launch browser
    ShellExecute(0, 'open', PChar('https://127.0.0.1'), '', '', 0);

    // terminate
    WriteLn('Hit any key to terminate.');

    ReadLn;
  finally
    Server.Active := False;
    Server.Free;
  end;
end;

begin
  try
    Start;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
