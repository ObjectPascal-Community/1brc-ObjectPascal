object dmChallengeWithFireDAC: TdmChallengeWithFireDAC
  Height = 293
  Width = 348
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
    Left = 208
    Top = 80
    object tblWeatherDataCityName: TStringField
      FieldName = 'CityName'
      Size = 50
    end
    object tblWeatherDataTemperature: TFloatField
      FieldName = 'Temperature'
    end
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 72
    Top = 32
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 80
    Top = 96
  end
  object FDLocalSQL: TFDLocalSQL
    SchemaName = 'measurements'
    Connection = FDConnection
    Active = True
    DataSets = <>
    Left = 80
    Top = 168
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
    Left = 200
    Top = 144
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
