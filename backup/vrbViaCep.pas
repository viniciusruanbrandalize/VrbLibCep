unit vrbViaCep;

{ -----------------------------------------------------------------------------
  MIT License

  Copyright (c) 2025 Vinícius Ruan Brandalize

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
------------------------------------------------------------------------------- }

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser, opensslsockets,
  Generics.Collections;

type
  TTipoBusca = (bCep, bLogradouro);

type
  TVrbEndereco =  class
  private
    FCep: String;
    FLogradouro: String;
    FComplemento: String;
    FBairro: String;
    FCidade: String;
    FUF:     String;
    FIBGE: String;
  public
    property CEP: String read FCep write FCep;
    property Logradouro: String read FLogradouro write FLogradouro;
    property Complemento: String read FComplemento write FComplemento;
    property Bairro: String read FBairro write FBairro;
    property Cidade: String read FCidade write FCidade;
    property UF: String read FUF write FUF;
    property IBGE: String read FIBGE write FIBGE;
  end;

type
  TListaVrbEndereco = TObjectList<TVrbEndereco>;

type

  { TVrbViaCep }

  TVrbViaCep = class(TComponent)
  private
    FTipoBusca: TTipoBusca;
    FURL:     String;
    FTodos:   String;
    FCep:     String;
    FLogradouro:  String;
    FComplemento: String;
    FBairro:  String;
    FCidade:  String;
    FUf:      String;
    FIBGE:    String;
    FErro:    String;
    FEndereco: TListaVrbEndereco;
    FGerarException: Boolean;
    FVersao:  String;
    FSobre:   String;
    procedure SetTipoBusca(AVAlue: TTipoBusca);
    procedure SetURL(AValue: String);
    procedure SetCep(AValue: String);
    procedure SetUF(AVAlue: String);
    procedure SetCidade(AValue: String);
    procedure SetLogradouro(AValue: String);
    procedure SetErro(AVAlue: String);
    procedure SetGerarException(AValue: Boolean);
    function RemoverCaracteres(texto: String): String;
    function FormatarCep(Texto: String): String;
    procedure LimparConsultaAnterior;
  protected

  public

    function Buscar: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CEP: String read FCep write SetCep;
    property Logradouro: String read FLogradouro write SetLogradouro;
    property Cidade: String read FCidade write SetCidade;
    property UF: String read FUf write SetUF;
    property Bairro: String read FBairro;
    property Complemento: String read FComplemento;
    property IBGE: String read FIBGE;
    property StrJSON: String read FTodos;
    property Erro: String read FErro;
    property Endereco: TListaVrbEndereco read FEndereco;

  published
    property TipoBusca: TTipoBusca read FTipoBusca write SetTipoBusca default bCep;
    property URL: String read FURL;
    property GerarException: Boolean read FGerarException write SetGerarException default True;
    property Versao: String read FVersao;
    property Sobre: String read FSobre;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VRB',[TVrbViaCep]);
end;

{ TVrbViaCep }

procedure TVrbViaCep.SetTipoBusca(AVAlue: TTipoBusca);
begin
  if FTipoBusca = AValue then
    Exit;
  FTipoBusca := AValue;
end;

procedure TVrbViaCep.SetURL(AValue: String);
begin
  if FURL = AValue then
    Exit;
  FURL := AValue;
end;

procedure TVrbViaCep.SetCep(AValue: String);
begin
  if FCep = AValue then
    Exit;
  FCep := AValue;
end;

procedure TVrbViaCep.SetUF(AVAlue: String);
begin
  if FUf = AValue then
    Exit;
  FUf := AValue;
end;

procedure TVrbViaCep.SetCidade(AValue: String);
begin
  if FCidade = AValue then
    Exit;
  FCidade := AValue;
end;

procedure TVrbViaCep.SetLogradouro(AValue: String);
begin
  if FLogradouro = AValue then
    Exit;
  FLogradouro := AValue;
end;

procedure TVrbViaCep.SetErro(AVAlue: String);
begin
  if FErro = AValue then
    Exit;
  FErro := AValue;
end;

procedure TVrbViaCep.SetGerarException(AValue: Boolean);
begin
  if FGerarException = AValue then
    Exit;
  FGerarException := AValue;
end;

function TVrbViaCep.RemoverCaracteres(texto: String): String;
begin
  texto := StringReplace(texto, '´', '', [rfReplaceAll]);
  texto := StringReplace(texto, '`', '', [rfReplaceAll]);
  texto := StringReplace(texto, '-', '', [rfReplaceAll]);
  texto := StringReplace(texto, '´', '', [rfReplaceAll]);
  texto := StringReplace(texto, '~', '', [rfReplaceAll]);
  texto := StringReplace(texto, '^', '', [rfReplaceAll]);
  texto := StringReplace(texto, '*', '', [rfReplaceAll]);
  texto := StringReplace(texto, '/', '', [rfReplaceAll]);
  texto := StringReplace(texto, '\', '', [rfReplaceAll]);
  texto := StringReplace(texto, '|', '', [rfReplaceAll]);
  texto := StringReplace(texto, '}', '', [rfReplaceAll]);
  texto := StringReplace(texto, '{', '', [rfReplaceAll]);
  texto := StringReplace(texto, '[', '', [rfReplaceAll]);
  texto := StringReplace(texto, ']', '', [rfReplaceAll]);
  texto := StringReplace(texto, '?', '', [rfReplaceAll]);
  texto := StringReplace(texto, '#', '', [rfReplaceAll]);
  texto := StringReplace(texto, '$', '', [rfReplaceAll]);
  texto := StringReplace(texto, '&', '', [rfReplaceAll]);
  texto := StringReplace(texto, '+', '', [rfReplaceAll]);
  texto := StringReplace(texto, '@', '', [rfReplaceAll]);
  texto := StringReplace(texto, '!', '', [rfReplaceAll]);
  texto := StringReplace(texto, ';', '', [rfReplaceAll]);
  texto := StringReplace(texto, ':', '', [rfReplaceAll]);
  texto := StringReplace(texto, '=', '', [rfReplaceAll]);
  texto := StringReplace(texto, '.', '', [rfReplaceAll]);
  texto := StringReplace(texto, ',', '', [rfReplaceAll]);
  texto := StringReplace(texto, '"', '', [rfReplaceAll]);
  texto := StringReplace(texto, 'á', 'a', [rfReplaceAll]);
  texto := StringReplace(texto, 'Á', 'A', [rfReplaceAll]);
  texto := StringReplace(texto, 'é', 'e', [rfReplaceAll]);
  texto := StringReplace(texto, 'É', 'E', [rfReplaceAll]);
  texto := StringReplace(texto, 'à', 'a', [rfReplaceAll]);
  texto := StringReplace(texto, 'À', 'A', [rfReplaceAll]);
  texto := StringReplace(texto, 'è', 'e', [rfReplaceAll]);
  texto := StringReplace(texto, 'È', 'E', [rfReplaceAll]);
  texto := StringReplace(texto, 'ç', 'c', [rfReplaceAll]);
  texto := StringReplace(texto, 'Ç', 'C', [rfReplaceAll]);
  texto := StringReplace(texto, 'í', 'i', [rfReplaceAll]);
  texto := StringReplace(texto, 'Í', 'I', [rfReplaceAll]);
  texto := StringReplace(texto, 'ì', 'i', [rfReplaceAll]);
  texto := StringReplace(texto, 'Ì', 'I', [rfReplaceAll]);
  texto := StringReplace(texto, 'ú', 'u', [rfReplaceAll]);
  texto := StringReplace(texto, 'Ú', 'U', [rfReplaceAll]);
  texto := StringReplace(texto, 'ù', 'u', [rfReplaceAll]);
  texto := StringReplace(texto, 'Ù', 'U', [rfReplaceAll]);
  texto := StringReplace(texto, 'ó', 'o', [rfReplaceAll]);
  texto := StringReplace(texto, 'Ó', 'O', [rfReplaceAll]);
  texto := StringReplace(texto, 'ò', 'o', [rfReplaceAll]);
  texto := StringReplace(texto, 'Ò', 'O', [rfReplaceAll]);
  texto := StringReplace(texto, 'ã', 'a', [rfReplaceAll]);
  texto := StringReplace(texto, 'Ã', 'A', [rfReplaceAll]);
  texto := StringReplace(texto, 'õ', 'o', [rfReplaceAll]);
  texto := StringReplace(texto, 'Õ', 'O', [rfReplaceAll]);
  texto := StringReplace(texto, '>', '', [rfReplaceAll]);
  texto := StringReplace(texto, '<', '', [rfReplaceAll]);
  texto := Trim(texto);
  Result := texto;
end;

function TVrbViaCep.FormatarCep(Texto: String): String;
var
  Valor: Integer;
begin
  Valor := 0;
  Texto := RemoverCaracteres(Texto);
  Valor := StrToIntDef(Texto, 0);
  Texto := FormatFloat('00000000', Valor);
  Result := Texto;
end;

procedure TVrbViaCep.LimparConsultaAnterior;
begin
  FTodos       := '';
  FBairro      := '';
  FCep         := '';
  FCidade      := '';
  FLogradouro  := '';
  FUf          := '';
  FComplemento := '';
  FIBGE        := '';
  FErro        := '';

  for i := 0 to Pred(FEndereco.Count) do
  begin
    FEndereco.Delete(i);
  end;

end;

function TVrbViaCep.Buscar: Boolean;
var
  Http: TFPHttpClient;
  Content: String;
  Json : TJSONData;
  i:Integer;
begin

  LimparConsultaAnterior();

  case FTipoBusca of
    bCep:
    begin

      Http:=TFPHttpClient.Create(nil);
      try

        try
          FCep := FormatarCep(FCep);
          Content := Http.Get(FURL + FCep + '/json/');
          Json := GetJSON(Content);
        except on ex:Exception do
          begin
            FErro  := ex.Message;
            Result := False;
            if FGerarException then
              raise Exception.Create(FErro);
            Exit;
          end;
        end;

        try
          try
            FTodos       := Content;
            FCEP         := Json.FindPath('cep').AsString;
            FLogradouro  := Json.FindPath('logradouro').AsString;
            FComplemento := Json.FindPath('complemento').AsString;
            FIBGE        := Json.FindPath('ibge').AsString;
            FBairro      := Json.FindPath('bairro').AsString;
            FCidade      := Json.FindPath('localidade').AsString;
            FUf          := Json.FindPath('uf').AsString;
          except on ex:Exception do
            begin
              FErro  := 'CEP não encontrado!';
              Result := False;
              if FGerarException then
                raise Exception.Create(FErro);
              Exit;
            end;
          end;
        finally
          Json.Free;
        end;
      finally
        Http.Free;
      end;

      Result := True;

    end;

    bLogradouro:
    begin

      Http:=TFPHttpClient.Create(nil);
      try

        try
          FUf         := RemoverCaracteres(FUf);
          FCidade     := RemoverCaracteres(FCidade);
          FLogradouro := RemoverCaracteres(FLogradouro);
          Content := Http.Get(FURL + UpperCase(FUf) + '/' + FCidade + '/' + FLogradouro + '/json/');
          Json := GetJSON(Content);
        except on ex:Exception do
          begin
            FErro  := ex.Message;
            Result := False;
            if FGerarException then
              raise Exception.Create(FErro);
            Exit;
          end;
        end;

        try
          try

            FTodos       := Content;
            FCep         :=  Json.Items[0].FindPath('cep').AsString;
            FLogradouro  :=  Json.Items[0].FindPath('logradouro').AsString;
            FComplemento :=  Json.Items[0].FindPath('complemento').AsString;
            FIBGE        :=  Json.Items[0].FindPath('ibge').AsString;
            FBairro      :=  Json.Items[0].FindPath('bairro').AsString;
            FCidade      :=  Json.Items[0].FindPath('localidade').AsString;
            FUf          :=  Json.Items[0].FindPath('uf').AsString;

            for i := 0 to Pred(Json.Count) do
            begin
              FEndereco.Add(TVrbEndereco.Create);
              FEndereco.Items[Pred(FEndereco.Count)].FCep         := Json.Items[i].FindPath('cep').AsString;
              FEndereco.Items[Pred(FEndereco.Count)].FLogradouro  := Json.Items[i].FindPath('logradouro').AsString;
              FEndereco.Items[Pred(FEndereco.Count)].FComplemento := Json.Items[i].FindPath('complemento').AsString;
              FEndereco.Items[Pred(FEndereco.Count)].FIBGE        := Json.Items[i].FindPath('ibge').AsString;
              FEndereco.Items[Pred(FEndereco.Count)].FBairro      := Json.Items[i].FindPath('bairro').AsString;
              FEndereco.Items[Pred(FEndereco.Count)].FCidade      := Json.Items[i].FindPath('localidade').AsString;
              FEndereco.Items[Pred(FEndereco.Count)].FUf          := Json.Items[i].FindPath('uf').AsString;
            end;

          except on ex:Exception do
            begin
              FErro  := 'CEP não encontrado!';
              Result := False;
              if FGerarException then
                raise Exception.Create(FErro);
              Exit;
            end;
          end;
        finally
          Json.Free;
        end;
      finally
        Http.Free;
      end;

      Result := True;

    end;
  end;
end;

constructor TVrbViaCep.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FURL    := 'https://viacep.com.br/ws/';
  FVersao := '1.0.0.0';
  FSobre  := 'Desenvolvido por Vinicius Ruan Brandalize';
  FGerarException := True;
  FEndereco := TListaVrbEndereco.Create;
end;

destructor TVrbViaCep.Destroy;
begin
  FEndereco.Free;
  inherited Destroy;
end;

end.
