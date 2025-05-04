unit demo;

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
    mRetorno.Lines.Add('UF: '+         VrbViaCep1.Endereco[0].UF);
    mRetorno.Lines.Add('Cód. IBGE: '+  VrbViaCep1.Endereco[0].IBGE);
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
      mRetorno.Lines.Add('UF: '+         VrbViaCep1.Endereco[i].UF);
      mRetorno.Lines.Add('Cód. IBGE: '+  VrbViaCep1.Endereco[i].IBGE);
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

