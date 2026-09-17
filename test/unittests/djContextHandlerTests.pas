(*
    Daraja HTTP Framework
    Copyright (c) 2016 Michael Justin

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.

    You can be released from the requirements of the license by purchasing
    a commercial license. Buying such a license is mandatory as soon as you
    develop commercial activities involving the Daraja framework without
    disclosing the source code of your own applications. These activities
    include: offering paid services to customers as an ASP, shipping Daraja
    with a closed source product.

*)

unit djContextHandlerTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjContextHandlerTests }

  TdjContextHandlerTests = class(TTestCase)
  published
    procedure TestRootContextMatchesAnyPath;
    procedure TestNamedContextMatchesItsPrefix;
    procedure TestNamedContextDoesNotMatchUnrelatedPath;
    procedure TestNamedContextDoesNotMatchPrefixOfAnotherName;
    procedure TestNamedContextRequiresTrailingSlashBoundary;
    procedure TestNoConnectorNamesMatchesAnyConnector;
    procedure TestConnectorNameWhitelistRejectsUnlistedConnector;
    procedure TestConnectorNameWhitelistAcceptsListedConnector;
  end;

implementation

uses
  djContextHandler;

type
  { exposes the protected ContextMatches for testing, as
    TTestPathMap does for TdjPathMap.GetSpecType in djPathMapTests.pas }
  TTestContextHandler = class(TdjContextHandler)
  public
    function ContextMatches(const ConnectorName, Target: string): Boolean;
  end;

function TTestContextHandler.ContextMatches(const ConnectorName, Target: string): Boolean;
begin
  Result := inherited;
end;

{ TdjContextHandlerTests }

procedure TdjContextHandlerTests.TestRootContextMatchesAnyPath;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('');
  try
    CheckTrue(Handler.ContextMatches('', '/'), '/');
    CheckTrue(Handler.ContextMatches('', '/anything'), '/anything');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextMatchesItsPrefix;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    CheckTrue(Handler.ContextMatches('', '/app/'), '/app/');
    CheckTrue(Handler.ContextMatches('', '/app/page.html'), '/app/page.html');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextDoesNotMatchUnrelatedPath;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    CheckFalse(Handler.ContextMatches('', '/other/'), '/other/');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextDoesNotMatchPrefixOfAnotherName;
var
  Handler: TTestContextHandler;
begin
  // 'app' must not match '/application/...' -- current implementation
  // checks Pos('/app/', Target) = 1, so this should already pass, but it
  // documents the boundary and guards a future regression (e.g. someone
  // "optimizing" to Pos('/app', Target) = 1).
  Handler := TTestContextHandler.Create('app');
  try
    CheckFalse(Handler.ContextMatches('', '/application/page.html'),
      '/application/page.html must not match context "app"');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextRequiresTrailingSlashBoundary;
var
  Handler: TTestContextHandler;
begin
  // documents current behavior for the bare context path with no trailing
  // slash -- worth confirming this is the intended contract, since the
  // servlet-style bare-prefix exception exists for TdjPathMap (see
  // djPathMapTests.TestUrlPattern, '/foo' matching '/foo/*') but
  // ContextMatches has no equivalent special case.
  Handler := TTestContextHandler.Create('app');
  try
    CheckFalse(Handler.ContextMatches('', '/app'),
      '/app (no trailing slash) with current Pos(''/app/'', Target) = 1 logic');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNoConnectorNamesMatchesAnyConnector;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    CheckTrue(Handler.ContextMatches('anything:8080', '/app/'),
      'empty ConnectorNames whitelist means match any connector');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestConnectorNameWhitelistRejectsUnlistedConnector;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    Handler.ConnectorNames.Add('127.0.0.1:8080');

    CheckFalse(Handler.ContextMatches('127.0.0.1:9090', '/app/'),
      'connector not in the whitelist must not match');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestConnectorNameWhitelistAcceptsListedConnector;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    Handler.ConnectorNames.Add('127.0.0.1:8080');

    CheckTrue(Handler.ContextMatches('127.0.0.1:8080', '/app/'),
      'connector in the whitelist must match');
  finally
    Handler.Free;
  end;
end;

end.
