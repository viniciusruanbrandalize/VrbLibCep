object frmDemo: TfrmDemo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Demonstra'#231#227'o VrbLibCep'
  ClientHeight = 384
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 15
  object pgc: TPageControl
    AlignWithMargins = True
    Left = 5
    Top = 208
    Width = 593
    Height = 171
    Cursor = crHandPoint
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = TabSheet1
    Align = alBottom
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Busca Por Cep'
      object btnBuscarPorCep: TButton
        Left = 480
        Top = 113
        Width = 89
        Height = 25
        Caption = 'Buscar'
        TabOrder = 0
        OnClick = btnBuscarPorCepClick
      end
      object edtCep: TLabeledEdit
        Left = 16
        Top = 40
        Width = 129
        Height = 23
        EditLabel.Width = 24
        EditLabel.Height = 15
        EditLabel.Caption = 'CEP:'
        MaxLength = 9
        TabOrder = 1
        Text = ''
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Busca Por Logradouro'
      ImageIndex = 1
      object edtLogradouro: TLabeledEdit
        Left = 11
        Top = 24
        Width = 542
        Height = 23
        EditLabel.Width = 65
        EditLabel.Height = 15
        EditLabel.Caption = 'Logradouro:'
        TabOrder = 0
        Text = ''
      end
      object edtCidade: TLabeledEdit
        Left = 11
        Top = 72
        Width = 382
        Height = 23
        EditLabel.Width = 40
        EditLabel.Height = 15
        EditLabel.Caption = 'Cidade:'
        TabOrder = 1
        Text = ''
      end
      object edtUf: TLabeledEdit
        Left = 407
        Top = 72
        Width = 81
        Height = 23
        EditLabel.Width = 17
        EditLabel.Height = 15
        EditLabel.Caption = 'UF:'
        TabOrder = 2
        Text = ''
      end
      object btnBuscarPorLogradouro: TButton
        Left = 480
        Top = 113
        Width = 91
        Height = 25
        Caption = 'Buscar'
        TabOrder = 3
        OnClick = btnBuscarPorLogradouroClick
      end
    end
  end
  object mRetorno: TMemo
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 593
    Height = 193
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BorderStyle = bsNone
    Lines.Strings = (
      'mRetorno')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitHeight = 171
  end
  object VrbViaCep1: TVrbViaCep
    Left = 104
    Top = 56
  end
end
