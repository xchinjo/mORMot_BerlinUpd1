object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 532
  ClientWidth = 981
  Caption = 'MainForm'
  WindowState = wsMaximized
  OldCreateOrder = False
  MonitoredKeys.Keys = <>
  PixelsPerInch = 96
  TextHeight = 13
  object UniPanel1: TUniPanel
    Left = 0
    Top = 0
    Width = 401
    Height = 532
    Hint = ''
    Align = alLeft
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    Caption = ''
    object uniImageSnapshot: TUniImage
      Left = 3
      Top = 1
      Width = 320
      Height = 240
      Hint = ''
      Stretch = True
      OnAjaxEvent = uniImageSnapshotAjaxEvent
    end
  end
  object UniPanel2: TUniPanel
    Left = 401
    Top = 0
    Width = 580
    Height = 532
    Hint = ''
    Align = alClient
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    Caption = 'UniPanel2'
    object UniHTMLFrame1: TUniHTMLFrame
      Left = 1
      Top = 1
      Width = 578
      Height = 530
      Hint = ''
      HTML.Strings = (
        '<body>'
        #9'<!-- Configure a few settings and attach camera -->'
        #9'<script language="JavaScript">'
        '// Grab elements, create settings, etc.'
        
          '   var video = document.getElementById('#39'uniImageSnapshot'#39') ;// d' +
          'ocument.getElementById('#39'video'#39');'
        ''
        '// Get access to the camera!'
        
          'if(navigator.mediaDevices && navigator.mediaDevices.getUserMedia' +
          ') {'
        '    // Not adding `{ audio: true }` since we only want video now'
        
          '    navigator.mediaDevices.getUserMedia({ video: true }).then(fu' +
          'nction(stream) {'
        '        video.src = window.URL.createObjectURL(stream);'
        '        video.play();'
        '    });'
        '}'
        ''
        ''
        '// Elements for taking the snapshot'
        'var canvas = document.getElementById('#39'canvas'#39');'
        'var context = canvas.getContext('#39'2d'#39');'
        'var video = document.getElementById('#39'video'#39');'
        ''
        '// Trigger photo take'
        
          'document.getElementById("snap").addEventListener("click", functi' +
          'on() {'
        #9'context.drawImage(video, 0, 0, 640, 480);'
        '   '
        '   '
        '   '
        '            '
        '      //   ajaxRequest(MainForm.uniImageSnapshot,'#39'UpdatePIC'#39',['
        '      //   '#39'PICDATA='#39'+video.src'
        '      //   ]);   '
        '   '
        '   '
        '});'
        ''
        '/* Legacy code below: getUserMedia '
        'else if(navigator.getUserMedia) { // Standard'
        '    navigator.getUserMedia({ video: true }, function(stream) {'
        '        video.src = stream;'
        '        video.play();'
        '    }, errBack);'
        '} else if(navigator.webkitGetUserMedia) { // WebKit-prefixed'
        
          '    navigator.webkitGetUserMedia({ video: true }, function(strea' +
          'm){'
        '        video.src = window.webkitURL.createObjectURL(stream);'
        '        video.play();'
        '    }, errBack);'
        '} else if(navigator.mozGetUserMedia) { // Mozilla-prefixed'
        '    navigator.mozGetUserMedia({ video: true }, function(stream){'
        '        video.src = window.URL.createObjectURL(stream);'
        '        video.play();'
        '    }, errBack);'
        '}'
        '*/'
        #9'</script>'
        #9
        #9'<!-- A button for taking snaps -->'
        ''
        ''
        '<video id="video" width="640" height="480" autoplay></video>'
        '<button id="snap">Snap Photo</button>'
        '<canvas id="canvas" width="640" height="480"></canvas>'
        #9
        '</body>')
      Align = alClient
      Anchors = [akLeft, akTop, akRight, akBottom]
    end
  end
end
