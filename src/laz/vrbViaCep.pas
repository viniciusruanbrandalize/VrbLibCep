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

  { TVrbEndereco }

  TVrbEndereco =  class
  private
    FCep: String;
    FLogradouro: String;
    FComplemento: String;
    FBairro: String;
    FCidade: String;
    FEstado: String;
    FUF:     String;
    FIBGE: String;
    FRegiao: String;
    FDDD: String;
    FGIA: String;
    FSIAFI: String;
    FUnidade: String;
  public
    constructor Create;
    destructor Destroy; override;
    property CEP: String read FCep write FCep;
    property Logradouro: String read FLogradouro write FLogradouro;
    property Complemento: String read FComplemento write FComplemento;
    property Bairro: String read FBairro write FBairro;
    property Cidade: String read FCidade write FCidade;
    property Estado: String read FEstado write FEstado;
    property UF: String read FUF write FUF;
    property IBGE: String read FIBGE write FIBGE;
    property Regiao: String read FRegiao write FRegiao;
    property DDD: String read FDDD write FDDD;
    property GIA: String read FGIA write FGIA;
    property SIAFI: String read FSIAFI write FSIAFI;
    property Unidade: String read FUnidade write FUnidade;
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
    FCidade:  String;
    FUf:      String;
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

    property StrJSON: String read FTodos;
    property Erro: String read FErro;
    property Endereco: TListaVrbEndereco read FEndereco;

  published
    property TipoBusca: TTipoBusca read FTipoBusca write SetTipoBusca default bCep;
    property URL: String read FURL;
    property CEP: String read FCep write SetCep;
    property Logradouro: String read FLogradouro write SetLogradouro;
    property Cidade: String read FCidade write SetCidade;
    property UF: String read FUf write SetUF;
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

{ TVrbEndereco }

constructor TVrbEndereco.Create;
begin
  //
end;

destructor TVrbEndereco.Destroy;
begin
  inherited Destroy;
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
  texto := StringReplace(texto, ' ', '%20', [rfReplaceAll]);
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
  FErro        := '';
  FEndereco.Clear;
end;

function TVrbViaCep.Buscar: Boolean;
var
  Http: TFPHttpClient;
  Content: String;
  Json : TJSONData;
  i:Integer;
begin

  LimparConsultaAnterior();

  Http:=TFPHttpClient.Create(nil);
  try

    try

      case FTipoBusca of
        bCep:
        begin
          FCep := FormatarCep(FCep);
          Content := Http.Get(FURL + FCep + '/json/');
        end;
        bLogradouro:
        begin
          FUf         := RemoverCaracteres(FUf);
          FCidade     := RemoverCaracteres(FCidade);
          FLogradouro := RemoverCaracteres(FLogradouro);
          Content := Http.Get(FURL + UpperCase(FUf) + '/' + FCidade + '/' +
                              FLogradouro + '/json/');
        end;
      end;

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

        FTodos := Content;

        if FTipoBusca = bLogradouro then
        begin
          for i := 0 to Pred(Json.Count) do
          begin
            FEndereco.Add(TVrbEndereco.Create);
            FEndereco[Pred(FEndereco.Count)].Cep         := Json.Items[i].FindPath('cep').AsString;
            FEndereco[Pred(FEndereco.Count)].Logradouro  := Json.Items[i].FindPath('logradouro').AsString;
            FEndereco[Pred(FEndereco.Count)].Complemento := Json.Items[i].FindPath('complemento').AsString;
            FEndereco[Pred(FEndereco.Count)].IBGE        := Json.Items[i].FindPath('ibge').AsString;
            FEndereco[Pred(FEndereco.Count)].Bairro      := Json.Items[i].FindPath('bairro').AsString;
            FEndereco[Pred(FEndereco.Count)].Cidade      := Json.Items[i].FindPath('localidade').AsString;
            FEndereco[Pred(FEndereco.Count)].Uf          := Json.Items[i].FindPath('uf').AsString;
            FEndereco[Pred(FEndereco.Count)].Unidade     := Json.Items[i].FindPath('unidade').AsString;
            FEndereco[Pred(FEndereco.Count)].DDD         := Json.Items[i].FindPath('ddd').AsString;
            FEndereco[Pred(FEndereco.Count)].Regiao      := Json.Items[i].FindPath('regiao').AsString;
            FEndereco[Pred(FEndereco.Count)].GIA         := Json.Items[i].FindPath('gia').AsString;
            FEndereco[Pred(FEndereco.Count)].SIAFI       := Json.Items[i].FindPath('siafi').AsString;
            FEndereco[Pred(FEndereco.Count)].Estado      := Json.Items[i].FindPath('estado').AsString;
          end;
        end
        else
        if TipoBusca = bCep then
        begin
          FEndereco.Add(TVrbEndereco.Create);
          FEndereco[Pred(FEndereco.Count)].Cep         := Json.FindPath('cep').AsString;
          FEndereco[Pred(FEndereco.Count)].Logradouro  := Json.FindPath('logradouro').AsString;
          FEndereco[Pred(FEndereco.Count)].Complemento := Json.FindPath('complemento').AsString;
          FEndereco[Pred(FEndereco.Count)].IBGE        := Json.FindPath('ibge').AsString;
          FEndereco[Pred(FEndereco.Count)].Bairro      := Json.FindPath('bairro').AsString;
          FEndereco[Pred(FEndereco.Count)].Cidade      := Json.FindPath('localidade').AsString;
          FEndereco[Pred(FEndereco.Count)].Uf          := Json.FindPath('uf').AsString;
          FEndereco[Pred(FEndereco.Count)].Unidade     := Json.FindPath('unidade').AsString;
          FEndereco[Pred(FEndereco.Count)].DDD         := Json.FindPath('ddd').AsString;
          FEndereco[Pred(FEndereco.Count)].Regiao      := Json.FindPath('regiao').AsString;
          FEndereco[Pred(FEndereco.Count)].GIA         := Json.FindPath('gia').AsString;
          FEndereco[Pred(FEndereco.Count)].SIAFI       := Json.FindPath('siafi').AsString;
          FEndereco[Pred(FEndereco.Count)].Estado      := Json.FindPath('estado').AsString;
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

constructor TVrbViaCep.Create(AOwner: TComponent);
begin
  FEndereco := TListaVrbEndereco.Create;
  inherited Create(AOwner);
  FURL    := 'https://viacep.com.br/ws/';
  FVersao := '1.0.0.0';
  FSobre  := 'Desenvolvido por Vinicius Ruan Brandalize';
  FGerarException := True;
end;

destructor TVrbViaCep.Destroy;
begin
  FEndereco.Free;
  inherited Destroy;
end;

end.
