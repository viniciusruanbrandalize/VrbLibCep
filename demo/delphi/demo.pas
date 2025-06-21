unit demo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Mask,
  Vcl.ExtCtrls, vrbViaCep;

type
  TfrmDemo = class(TForm)
    pgc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    mRetorno: TMemo;
    btnBuscarPorCep: TButton;
    edtCep: TLabeledEdit;
    VrbViaCep1: TVrbViaCep;
    edtLogradouro: TLabeledEdit;
    edtCidade: TLabeledEdit;
    edtUf: TLabeledEdit;
    btnBuscarPorLogradouro: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnBuscarPorCepClick(Sender: TObject);
    procedure btnBuscarPorLogradouroClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.dfm}

procedure TfrmDemo.btnBuscarPorCepClick(Sender: TObject);
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

procedure TfrmDemo.btnBuscarPorLogradouroClick(Sender: TObject);
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

procedure TfrmDemo.FormShow(Sender: TObject);
begin
  mRetorno.Lines.Clear;
end;

end.
