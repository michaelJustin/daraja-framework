(*
    Daraja Framework
    Copyright (C) 2016 Michael Justin

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    You can be released from the requirements of the license by purchasing
    a commercial license. Buying such a license is mandatory as soon as you
    develop commercial activities involving the Daraja framework without
    disclosing the source code of your own applications. These activities
    include: offering paid services to customers as an ASP, shipping Daraja 
    with a closed source product.

*)

unit djWebComponentHandlerTests;

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type
  TdjWebComponentHandlerTests = class(TTestCase)
  published
    // procedure TestAV;
    procedure TestAddPathMap;
    procedure TestAddTwoComponents;
    procedure TestAddSameComponentWithDifferentPaths;
    procedure TestTwoContextsFails;
    procedure TestAddSamePathMapTwiceFails;
    procedure TestTwoComponentsSameNameFails;
    procedure TestTwoComponentsSamePathMapFails;
  end;

implementation

uses
  Classes, SysUtils,
  djWebComponentHolder, IdCustomHTTPServer, djWebComponent, djWebAppContext,
  djWebComponentHandler, djInterfaces;

type
  TExamplePage = class(TdjWebComponent)
  public
    procedure OnGet(Request: TIdHTTPRequestInfo; Response:
      TIdHTTPResponseInfo); override;
  end;

  TOtherPage = class(TdjWebComponent)
  public
  end;

  { TExamplePage }

procedure TExamplePage.OnGet(Request: TIdHTTPRequestInfo;
  Response: TIdHTTPResponseInfo);
begin
  inherited;

end;

type
  TTestdjWebComponentHandler = class(TdjWebComponentHandler)
  public
    function FindComponent(const ATarget: string): TdjWebComponentHolder;
  end;

{ TTestdjWebComponentHandler }

function TTestdjWebComponentHandler.FindComponent(const
  ATarget: string): TdjWebComponentHolder;
begin
  Result := inherited;
end;

procedure TdjWebComponentHandlerTests.TestAddTwoComponents;
var
  Context: TdjWebAppContext;
  H1, H2: TdjWebComponentHolder;
  Handler: TTestdjWebComponentHandler;
begin
  Context := TdjWebAppContext.Create('');
  try
    Handler := TTestdjWebComponentHandler.Create;
    try
      CheckTrue(Handler.Stopped);
      Handler.Start;

      // add a web component using Holder
      H1 := TdjWebComponentHolder.Create(TExamplePage);

      H1.Name := 'Example Page';
      H1.SetInitParameter('a', '123');

      H1.SetContext(Context.GetCurrentContext);

      Handler.AddWithMapping(H1, '/index.html');
      CheckEquals(1, Handler.WebComponents.Count);

      H2 := TdjWebComponentHolder.Create(TExamplePage);
      H2.SetContext(Context.GetCurrentContext);
      // CheckEquals('TExamplePage', H2.Name);
      // todo not allowed H2.Name := 'Example Page';
      H2.SetInitParameter('b', '456');

      // todo same mapping not allowed
      Handler.AddWithMapping(H2, '/index2.html');
      CheckEquals(2, Handler.WebComponents.Count);

      // did it find the holder?
      CheckEquals(H1.Name, Handler.FindComponent('/index.html').Name);
      CheckEquals('Example Page', Handler.FindComponent('/index.html').Name);

      // Handler.Stop;

    finally
      Handler.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebComponentHandlerTests.TestAddPathMap;
var
  Context: TdjWebAppContext;
  H1: TdjWebComponentHolder;
  Handler: TTestdjWebComponentHandler;
begin
  Context := TdjWebAppContext.Create('');
  try
    Handler := TTestdjWebComponentHandler.Create;
    try
      H1 := Handler.CreateHolder(TExamplePage);
      H1.SetContext(Context.GetCurrentContext);

      Handler.AddWithMapping(H1, '/a');
      Handler.AddWithMapping(H1, '/b');

      CheckEquals(2, Handler.WebComponentMappings[0].PathSpecs.Count);
      CheckEquals('/a,/b', Handler.WebComponentMappings[0].PathSpecs.CommaText);

    finally
      Handler.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebComponentHandlerTests.TestAddSamePathMapTwiceFails;
var
  Context: TdjWebAppContext;
  H1: TdjWebComponentHolder;
  Handler: TTestdjWebComponentHandler;
begin
  Context := TdjWebAppContext.Create('');
  try
    Handler := TTestdjWebComponentHandler.Create;
    try
      H1 := TdjWebComponentHolder.Create(TExamplePage);
      H1.SetContext(Context.GetCurrentContext);

      Handler.AddWithMapping(H1, '/index.html');

      try
        // add the same path map
        Handler.AddWithMapping(H1, '/index.html');
      except
        on E: EWebComponentException do
        begin
          // expected exception
        end;
        on E: Exception do
        begin
          Fail(E.Message);
        end;
      end;
    finally
      Handler.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebComponentHandlerTests.TestTwoComponentsSamePathMapFails;
var
  Context: TdjWebAppContext;
  H1, H2: TdjWebComponentHolder;
  Handler: TTestdjWebComponentHandler;
begin
  Context := TdjWebAppContext.Create('');
  try
    Handler := TTestdjWebComponentHandler.Create;
    try
      H1 := TdjWebComponentHolder.Create(TExamplePage);
      H1.SetContext(Context.GetCurrentContext);

      Handler.AddWithMapping(H1, '/index.html');

      H2 := TdjWebComponentHolder.Create(TExamplePage);
      try
        H2.SetContext(Context.GetCurrentContext);

        // add the same path map
        // Expected Exception := EWebComponentException;
        try
          Handler.AddWithMapping(H2, '/index.html');
        except
          on E: EWebComponentException do
          begin
            // expected exception
          end;
          on E: Exception do
          begin
            Fail(E.Message);
          end;
        end;
      finally
        H2.Free;
      end;
    finally
      Handler.Free;
    end;
  finally
    Context.Free;
  end;
end;



procedure TdjWebComponentHandlerTests.TestTwoComponentsSameNameFails;
var
  Context: TdjWebAppContext;
  H1, H2: TdjWebComponentHolder;
  Handler: TTestdjWebComponentHandler;
begin
  Context := TdjWebAppContext.Create('');
  try
    Handler := TTestdjWebComponentHandler.Create;
    try
      H1 := Handler.CreateHolder(TExamplePage);
      H1.SetContext(Context.GetCurrentContext);
      H1.Name := 'SameNameFails';
      Handler.AddWithMapping(H1, '/a.html');

      H2 := Handler.CreateHolder(TOtherPage);
      try
        H2.SetContext(Context.GetCurrentContext);
        H2.Name := 'SameNameFails';

        // add the same name
        // Expected Exception := EWebComponentException;
        try
          Handler.AddWithMapping(H2, '/b.html');
        except
          on E: EWebComponentException do
          begin
            // expected exception
          end;
          on E: Exception do
          begin
            Fail(E.Message);
          end;
        end;
      finally
        H2.Free;
      end;

    finally
      Handler.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebComponentHandlerTests.TestTwoContextsFails;
var
  C1, C2: TdjWebAppContext;
  H1, H2: TdjWebComponentHolder;
  Handler: TTestdjWebComponentHandler;
begin
  C1 := TdjWebAppContext.Create('');
  C2 := TdjWebAppContext.Create('');
  try
    Handler := TTestdjWebComponentHandler.Create;
    try
      H1 := Handler.CreateHolder(TExamplePage);
      H1.SetContext(C1.GetCurrentContext);
      Handler.AddWithMapping(H1, '/a.html');

      H2 := Handler.CreateHolder(TOtherPage);
      try
        H2.SetContext(C2.GetCurrentContext);

        // different context fails
        // ExpectedException := EWebComponentException;
        try
          Handler.AddWithMapping(H2, '/b.html');
        except
          on E: EWebComponentException do
          begin
            // expected exception
          end;
          on E: Exception do
          begin
            Fail(E.Message);
          end;
      end;
      finally
        H2.Free;
      end;

    finally
      Handler.Free;
    end;
  finally
    C1.Free;
    C2.Free;
  end;
end;

procedure TdjWebComponentHandlerTests.TestAddSameComponentWithDifferentPaths;
var
  Context: TdjWebAppContext;
  H1: TdjWebComponentHolder;
  Handler: TTestdjWebComponentHandler;
begin
  Context := TdjWebAppContext.Create('');
  try
    Handler := TTestdjWebComponentHandler.Create;
    try
      H1 := TdjWebComponentHolder.Create(TExamplePage);
      H1.SetContext(Context.GetCurrentContext);

      Handler.AddWithMapping(H1, '/index.html');
      // add the same component with different path map
      Handler.AddWithMapping(H1, '/other.html');
    finally
      Handler.Free;
    end;
  finally
    Context.Free;
  end;
end;

end.

