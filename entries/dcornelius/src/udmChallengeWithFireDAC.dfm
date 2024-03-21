object dmChallengeWithFireDAC: TdmChallengeWithFireDAC
  Height = 366
  Width = 435
  PixelsPerInch = 120
  object tblWeatherData: TFDMemTable
    ActiveStoredUsage = [auDesignTime]
    Active = True
    FieldDefs = <
      item
        Name = 'CityName'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Temperature'
        DataType = ftFloat
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    LocalSQL = FDLocalSQL
    StoreDefs = True
    Left = 260
    Top = 100
    object tblWeatherDataCityName: TStringField
      FieldName = 'CityName'
      Size = 50
    end
    object tblWeatherDataTemperature: TFloatField
      FieldName = 'Temperature'
    end
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 90
    Top = 40
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 100
    Top = 120
  end
  object FDLocalSQL: TFDLocalSQL
    SchemaName = 'measurements'
    Connection = FDConnection
    Active = True
    DataSets = <>
    Left = 100
    Top = 210
  end
  object qryCityTemps: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    Connection = FDConnection
    FetchOptions.AssignedValues = [evMode, evRecordCountMode, evUnidirectional, evCursorKind]
    FetchOptions.Mode = fmAll
    FetchOptions.CursorKind = ckForwardOnly
    FetchOptions.Unidirectional = True
    SQL.Strings = (
      'select CityName, '
      '  Min(Temperature) as MinTemp, '
      '  Max(Temperature) as MaxTemp,'
      '  Sum(Temperature) as SumTemp,'
      '  Count(Temperature) as TempCount'
      'from measurements.tblWeatherData'
      'group by CityName'
      'order by CityName')
    Left = 250
    Top = 180
    object qryCityTempsCityName: TStringField
      FieldName = 'CityName'
      Origin = 'CityName'
      Size = 50
    end
    object qryCityTempsTempCount: TLargeintField
      AutoGenerateValue = arDefault
      FieldName = 'TempCount'
      Origin = 'TempCount'
      ProviderFlags = []
      ReadOnly = True
    end
    object qryCityTempsMinTemp: TFloatField
      FieldName = 'MinTemp'
    end
    object qryCityTempsMaxTemp: TFloatField
      FieldName = 'MaxTemp'
    end
    object qryCityTempsSumTemp: TFloatField
      FieldName = 'SumTemp'
    end
  end
end
