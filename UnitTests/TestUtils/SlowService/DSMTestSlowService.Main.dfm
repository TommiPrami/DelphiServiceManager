object DSMTestSlow: TDSMTestSlow
  AllowPause = False
  DisplayName = 'DSM Test Slow Service'
  StartType = stManual
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
