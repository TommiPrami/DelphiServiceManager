object DSMTestDependent: TDSMTestDependent
  AllowPause = False
  Dependencies = <
    item
      Name = 'DSMTestFast'
      IsGroup = False
    end>
  DisplayName = 'DSM Test Dependent Service'
  StartType = stManual
  Height = 150
  Width = 215
end
