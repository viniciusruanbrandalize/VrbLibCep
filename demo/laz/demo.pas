unit demo;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, vrbViaCep;

type

  { TfrmDemo }

  TfrmDemo = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtCep: TLabeledEdit;
    edtLogradouro: TLabeledEdit;
    edtCidade: TLabeledEdit;
    edtUf: TLabeledEdit;
    mRetorno: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    VrbViaCep1: TVrbViaCep;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.lfm}

{ TfrmDemo }

procedure TfrmDemo.FormShow(Sender: TObject);
begin
  mRetorno.Lines.Clear;
  edtCep.Clear;
  edtLogradouro.Clear;
  edtUf.Clear;
  edtCidade.Clear;
end;

procedure TfrmDemo.Button1Click(Sender: TObject);
begin
  mRetorno.Lines.Clear;
  VrbViaCep1.TipoBusca := bCep;
  VrbViaCep1.CEP       := edtCep.Text;
  if VrbViaCep1.Buscar then
  begin
    mRetorno.Lines.Add('JSON: '+       VrbViaCep1.StrJSON);
    mRetorno.Lines.Add('CEP: '+        VrbViaCep1.Endereco[0].CEP);
    mRetorno.Lines.Add('Logradouro: '+ VrbViaCep1.Endereco[0].Logradouro);
    mRetorno.Lines.Add('Complemento: '+VrbViaCep1.Endereco[0].Complemento);
    mRetorno.Lines.Add('Bairro: '+     VrbViaCep1.Endereco[0].Bairro);
    mRetorno.Lines.Add('Cidade: '+     VrbViaCep1.Endereco[0].Cidade);
    mRetorno.Lines.Add('Estado: '+     VrbViaCep1.Endereco[0].Estado);
    mRetorno.Lines.Add('UF: '+         VrbViaCep1.Endereco[0].UF);
    mRetorno.Lines.Add('Cód. IBGE: '+  VrbViaCep1.Endereco[0].IBGE);
    mRetorno.Lines.Add('Região: '+     VrbViaCep1.Endereco[0].Regiao);
    mRetorno.Lines.Add('DDD: '+        VrbViaCep1.Endereco[0].DDD);
    mRetorno.Lines.Add('GIA: '+        VrbViaCep1.Endereco[0].GIA);
    mRetorno.Lines.Add('SIAFI: '+      VrbViaCep1.Endereco[0].SIAFI);
    mRetorno.Lines.Add('Unidade: '+    VrbViaCep1.Endereco[0].Unidade);
  end
  else
  begin
    mRetorno.Lines.Add(VrbViaCep1.Erro);
    mRetorno.Lines.Add(VrbViaCep1.StrJSON);
  end;
end;

procedure TfrmDemo.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  mRetorno.Lines.Clear;
  VrbViaCep1.TipoBusca  := bLogradouro;
  VrbViaCep1.Logradouro := edtLogradouro.Text;
  VrbViaCep1.Cidade     := edtCidade.Text;
  VrbViaCep1.UF         := edtUf.Text;
  if VrbViaCep1.Buscar then
  begin

    mRetorno.Lines.Add('JSON: '+       VrbViaCep1.StrJSON);

    for i := 0 to Pred(VrbViaCep1.Endereco.Count) do
    begin
      mRetorno.Lines.Add('CEP: '+        VrbViaCep1.Endereco[i].CEP);
      mRetorno.Lines.Add('Logradouro: '+ VrbViaCep1.Endereco[i].Logradouro);
      mRetorno.Lines.Add('Complemento: '+VrbViaCep1.Endereco[i].Complemento);
      mRetorno.Lines.Add('Bairro: '+     VrbViaCep1.Endereco[i].Bairro);
      mRetorno.Lines.Add('Cidade: '+     VrbViaCep1.Endereco[i].Cidade);
      mRetorno.Lines.Add('Estado: '+     VrbViaCep1.Endereco[i].Estado);
      mRetorno.Lines.Add('UF: '+         VrbViaCep1.Endereco[i].UF);
      mRetorno.Lines.Add('Região: '+     VrbViaCep1.Endereco[i].Regiao);
      mRetorno.Lines.Add('Cód. IBGE: '+  VrbViaCep1.Endereco[i].IBGE);
      mRetorno.Lines.Add('DDD: '+        VrbViaCep1.Endereco[i].DDD);
      mRetorno.Lines.Add('GIA: '+        VrbViaCep1.Endereco[i].GIA);
      mRetorno.Lines.Add('SIAFI: '+      VrbViaCep1.Endereco[i].SIAFI);
      mRetorno.Lines.Add('Unidade: '+    VrbViaCep1.Endereco[i].Unidade);
      mRetorno.Lines.Add('');
    end;

  end
  else
  begin
    mRetorno.Lines.Add(VrbViaCep1.Erro);
    mRetorno.Lines.Add(VrbViaCep1.StrJSON);
  end;
end;

end.

