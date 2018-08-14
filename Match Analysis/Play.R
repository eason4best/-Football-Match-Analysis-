library("shiny")
library("shinydashboard")
library("ggplot2")
library("jpeg")
library("grid")
library("gridExtra")  
library("viridis")



ui=dashboardPage(
  skin="blue", 
  dashboardHeader(title="MATCH ANALYSIS"),
  dashboardSidebar(
    selectInput(inputId="team",label="Team",choices=unique(team_player$Team)),
    selectInput(inputId="player",label="Player",choices="",selected="")
  ),
  dashboardBody(
    fluidRow(
      box(imageOutput(outputId="playerphoto",width="100%"),status="primary",solidHeader=TRUE,align="center",width=3,height=225,background=NULL),
      valueBoxOutput(outputId="pass",width=3),
      valueBoxOutput(outputId="shot",width=3),
      valueBoxOutput(outputId="aerial_won",width=3),
      valueBoxOutput(outputId="passing_accuracy",width=3),
      valueBoxOutput(outputId="tackle",width=3),
      valueBoxOutput(outputId="touch",width=3)
    ),
   fluidRow(
     plotOutput(outputId="heatmap",width="100%"),align="center",width=6
     )
  )
  )


server=function(session,input,output){
  output$playerphoto=renderImage({
    if(input$player=="HARRY KANE"){            
      list(src="PlayerData/Profile Picture/England/HARRY KANE.png",height=200,width=191.5)
    }
    else if(input$player=="KIERAN TRIPPIER"){            
      list(src="PlayerData/Profile Picture/England/KIERAN TRIPPIER.png",height=200,width=191.5)
    }
    else if(input$player=="DELE ALLI"){            
      list(src="PlayerData/Profile Picture/England/DELE ALLI.png",height=200,width=191.5)
    }
    else if(input$player=="JORDAN PICKFORD"){            
      list(src="PlayerData/Profile Picture/England/JORDAN PICKFORD.png",height=200,width=191.5)
    }
    else if(input$player=="JESSE LINGARD"){            
      list(src="PlayerData/Profile Picture/England/JESSE LINGARD.png",height=200,width=191.5)
    }
    else if(input$player=="JOHN STONES"){            
      list(src="PlayerData/Profile Picture/England/JOHN STONES.png",height=200,width=191.5)
    }
    else if(input$player=="ASHLEY YOUNG"){            
      list(src="PlayerData/Profile Picture/England/ASHLEY YOUNG.png",height=200,width=191.5)
    }
    else if(input$player=="JORDAN HENDERSON"){            
      list(src="PlayerData/Profile Picture/England/JORDAN HENDERSON.png",height=200,width=191.5)
    }
    else if(input$player=="RAHEEM STERLING"){            
      list(src="PlayerData/Profile Picture/England/RAHEEM STERLING.png",height=200,width=191.5)
    }
    else if(input$player=="KYLE WALKER"){            
      list(src="PlayerData/Profile Picture/England/KYLE WALKER.png",height=200,width=191.5)
    }
    else if(input$player=="HARRY MAGUIRE"){            
      list(src="PlayerData/Profile Picture/England/HARRY MAGUIRE.png",height=200,width=191.5)
    }
    else if(input$player=="DANIJEL SUBASIC"){            
      list(src="PlayerData/Profile Picture/Croatia/DANIJEL SUBASIC.png",height=215,width=142)
    }
    else if(input$player=="DOMAGOJ VIDA"){            
      list(src="PlayerData/Profile Picture/Croatia/DOMAGOJ VIDA.png",height=215,width=142)
    }
    else if(input$player=="IVAN PERISIC"){            
      list(src="PlayerData/Profile Picture/Croatia/IVAN PERISIC.png",height=215,width=142)
    }
    else if(input$player=="IVAN RAKITIC"){            
      list(src="PlayerData/Profile Picture/Croatia/IVAN RAKITIC.png",height=215,width=142)
    }
    else if(input$player=="LUKA MODRIC"){            
      list(src="PlayerData/Profile Picture/Croatia/LUKA MODRIC.png",height=215,width=142)
    } 
    else if(input$player=="MARCELO BROZOVIC"){            
      list(src="PlayerData/Profile Picture/Croatia/MARCELO BROZOVIC.png",height=215,width=142)
    }
    else if(input$player=="MARIO MANDZUKIC"){            
      list(src="PlayerData/Profile Picture/Croatia/MARIO MANDZUKIC.png",height=215,width=142)
    }
    else if(input$player=="MATEO KOVACIC"){            
      list(src="PlayerData/Profile Picture/Croatia/MATEO KOVACIC.png",height=215,width=142)
    }
    else if(input$player=="IVAN STRINIC"){            
      list(src="PlayerData/Profile Picture/Croatia/IVAN STRINIC.png",height=215,width=142)
    }
    else if(input$player=="SIME VRSALJKO"){            
      list(src="PlayerData/Profile Picture/Croatia/SIME VRSALJKO.png",height=215,width=142)
    }
    else if(input$player=="DEJAN LOVRAN"){            
      list(src="PlayerData/Profile Picture/Croatia/DEJAN LOVRAN.png",height=215,width=142)
    }
    else{
      list(src="PlayerData/Profile Picture/England/GARY CAHILL.png",height=200,width=191.5)
    }
      },deleteFile=FALSE)
  output$pass=renderValueBox({
  if(input$player=="HARRY KANE"){
    valueBox(
     team_player$Pass[1],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="KIERAN TRIPPIER"){
    valueBox(
      team_player$Pass[2],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="RAHEEM STERLING"){
    valueBox(
      team_player$Pass[3],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="DELE ALLI"){
    valueBox(
      team_player$Pass[4],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="JESSE LINGARD"){
    valueBox(
      team_player$Pass[5],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="JORDAN HENDERSON"){
    valueBox(
      team_player$Pass[6],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="HARRY MAGUIRE"){
    valueBox(
      team_player$Pass[7],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="JOHN STONES"){
    valueBox(
      team_player$Pass[8],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="KYLE WALKER"){
    valueBox(
      team_player$Pass[9],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="ASHLEY YOUNG"){
    valueBox(
      team_player$Pass[10],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="MARIO MANDZUKIC"){
    valueBox(
      team_player$Pass[12],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="MATEO KOVACIC"){
    valueBox(
      team_player$Pass[13],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="LUKA MODRIC"){
    valueBox(
      team_player$Pass[14],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="IVAN RAKITIC"){
    valueBox(
      team_player$Pass[15],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="IVAN PERISIC"){
    valueBox(
      team_player$Pass[16],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="NIKOLA KALINIC"){
    valueBox(
      team_player$Pass[17],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="SIME VRSALJKO"){
    valueBox(
      team_player$Pass[18],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="MARCELO BROZOVIC"){
    valueBox(
      team_player$Pass[19],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="DOMAGOJ VIDA"){
    valueBox(
      team_player$Pass[20],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="VEDRAN CORLUKA"){
    valueBox(
      team_player$Pass[21],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }else if(input$player=="DANIJEL SUBASIC"){
    valueBox(
      team_player$Pass[22],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }
  else{
    valueBox(
      team_player$Pass[11],
      "傳球",
      icon=icon("futbol-o"),
      color="blue"
    )
  }
  })
  
  
  
output$shot=renderValueBox({
if(input$player=="HARRY KANE"){
      valueBox(
       team_player$Shot[1],
        "嘗試射門",
        icon=icon("crosshairs",class="fa-spin"),
        color="red"
      )
}else if(input$player=="KIERAN TRIPPIER"){
  valueBox(
    team_player$Shot[2],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="RAHEEM STERLING"){
  valueBox(
    team_player$Shot[3],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="DELE ALLI"){
  valueBox(
    team_player$Shot[4],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="JESSE LINGARD"){
  valueBox(
    team_player$Shot[5],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="JORDAN HENDERSON"){
  valueBox(
    team_player$Shot[6],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="HARRY MAGUIRE"){
  valueBox(
    team_player$Shot[7],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="JOHN STONES"){
  valueBox(
    team_player$Shot[8],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="KYLE WALKER"){
  valueBox(
    team_player$Shot[9],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="ASHLEY YOUNG"){
  valueBox(
    team_player$Shot[10],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="MARIO MANDZUKIC"){
  valueBox(
    team_player$Shot[12],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="MATEO KOVACIC"){
  valueBox(
    team_player$Shot[13],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="LUKA MODRIC"){
  valueBox(
    team_player$Shot[14],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="IVAN RAKITIC"){
  valueBox(
    team_player$Shot[15],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="IVAN PERISIC"){
  valueBox(
    team_player$Shot[16],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="NIKOLA KALINIC"){
  valueBox(
    team_player$Shot[17],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="SIME VRSALJKO"){
  valueBox(
    team_player$Shot[18],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="MARCELO BROZOVIC"){
  valueBox(
    team_player$Shot[19],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="DOMAGOJ VIDA"){
  valueBox(
    team_player$Shot[20],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="VEDRAN CORLUKA"){
  valueBox(
    team_player$Shot[21],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}else if(input$player=="DANIJEL SUBASIC"){
  valueBox(
    team_player$Shot[22],
    "嘗試射門",
    icon=icon("crosshairs",class="fa-spin"),
    color="red"
  )
}
  else{
      valueBox(
       team_player$Shot[11],
        "嘗試射門",
        icon=icon("crosshairs",class="fa-spin"),
        color="red"
      )
    }
  })



  output$aerial_won=renderValueBox({
    if(input$player=="HARRY KANE"){
      valueBox(
        team_player$Aerial.Won[1],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }
    else if(input$player=="HARRY KANE"){
      valueBox(
        team_player$Aerial.Won[1],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="KIERAN TRIPPIER"){
      valueBox(
        team_player$Aerial.Won[2],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="RAHEEM STERLING"){
      valueBox(
        team_player$Aerial.Won[3],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="DELE ALLI"){
      valueBox(
        team_player$Aerial.Won[4],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="JESSE LINGARD"){
      valueBox(
        team_player$Aerial.Won[5],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="JORDAN HENDERSON"){
      valueBox(
        team_player$Aerial.Won[6],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="HARRY MAGUIRE"){
      valueBox(
        team_player$Aerial.Won[7],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="JOHN STONES"){
      valueBox(
        team_player$Aerial.Won[8],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="KYLE WALKER"){
      valueBox(
        team_player$Aerial.Won[9],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="ASHLEY YOUNG"){
      valueBox(
        team_player$Aerial.Won[10],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="MARIO MANDZUKIC"){
      valueBox(
        team_player$Aerial.Won[12],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="MATEO KOVACIC"){
      valueBox(
        team_player$Aerial.Won[13],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="LUKA MODRIC"){
      valueBox(
        team_player$Aerial.Won[14],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="IVAN RAKITIC"){
      valueBox(
        team_player$Aerial.Won[15],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="IVAN PERISIC"){
      valueBox(
        team_player$Aerial.Won[16],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="NIKOLA KALINIC"){
      valueBox(
        team_player$Aerial.Won[17],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="SIME VRSALJKO"){
      valueBox(
        team_player$Aerial.Won[18],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="MARCELO BROZOVIC"){
      valueBox(
        team_player$Aerial.Won[19],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="DOMAGOJ VIDA"){
      valueBox(
        team_player$Aerial.Won[20],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="VEDRAN CORLUKA"){
      valueBox(
        team_player$Aerial.Won[21],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else if(input$player=="DANIJEL SUBASIC"){
      valueBox(
        team_player$Aerial.Won[22],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }else{
      valueBox(
        team_player$Aerial.Won[11],
        "爭頂成功",
        icon=icon("plane"),
        color="aqua"
      )
    }
  })
  
  
  
output$passing_accuracy=renderValueBox({
    if(input$player=="HARRY KANE"){
      valueBox(
        paste(team_player$Passing.Accuracy[1],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="KIERAN TRIPPIER"){
      valueBox(
        paste(team_player$Passing.Accuracy[2],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="RAHEEM STERLING"){
      valueBox(
        paste(team_player$Passing.Accuracy[3],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="DELE ALLI"){
      valueBox(
        paste(team_player$Passing.Accuracy[4],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="JESSE LINGARD"){
      valueBox(
        paste(team_player$Passing.Accuracy[5],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="JORDAN HENDERSON"){
      valueBox(
        paste(team_player$Passing.Accuracy[6],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="HARRY MAGUIRE"){
      valueBox(
        paste(team_player$Passing.Accuracy[7],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="JOHN STONES"){
      valueBox(
        paste(team_player$Passing.Accuracy[8],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="KYLE WALKER"){
      valueBox(
        paste(team_player$Passing.Accuracy[9],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="ASHLEY YOUNG"){
      valueBox(
        paste(team_player$Passing.Accuracy[10],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="MARIO MANDZUKIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[12],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="MATEO KOVACIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[13],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="LUKA MODRIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[14],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="IVAN RAKITIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[15],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="IVAN PERISIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[16],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="NIKOLA KALINIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[17],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="SIME VRSALJKO"){
      valueBox(
        paste(team_player$Passing.Accuracy[18],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="MARCELO BROZOVIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[19],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="DOMAGOJ VIDA"){
      valueBox(
        paste(team_player$Passing.Accuracy[20],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="VEDRAN CORLUKA"){
      valueBox(
        paste(team_player$Passing.Accuracy[21],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else if(input$player=="DANIJEL SUBASIC"){
      valueBox(
        paste(team_player$Passing.Accuracy[22],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }else{
      valueBox(
        paste(team_player$Passing.Accuracy[11],"%",sep=" "),
        "傳球成功率",
        icon=icon("sort-numeric-asc"),
        color="green"
      )
    }
  })



output$tackle=renderValueBox({
    if(input$player=="HARRY KANE"){
      valueBox(
       team_player$Tackle[1],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="KIERAN TRIPPIER"){
      valueBox(
        team_player$Tackle[2],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="RAHEEM STERLING"){
      valueBox(
        team_player$Tackle[3],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="DELE ALLI"){
      valueBox(
        team_player$Tackle[4],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="JESSE LINGARD"){
      valueBox(
        team_player$Tackle[5],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="JORDAN HENDERSON"){
      valueBox(
        team_player$Tackle[6],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="HARRY MAGUIRE"){
      valueBox(
        team_player$Tackle[7],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="JOHN STONES"){
      valueBox(
        team_player$Tackle[8],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="KYLE WALKER"){
      valueBox(
        team_player$Tackle[9],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="ASHLEY YOUNG"){
      valueBox(
        team_player$Tackle[10],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="MARIO MANDZUKIC"){
      valueBox(
        team_player$Tackle[12],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="MATEO KOVACIC"){
      valueBox(
        team_player$Tackle[13],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="LUKA MODRIC"){
      valueBox(
        team_player$Tackle[14],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="IVAN RAKITIC"){
      valueBox(
        team_player$Tackle[15],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="IVAN PERISIC"){
      valueBox(
        team_player$Tackle[16],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="NIKOLA KALINIC"){
      valueBox(
        team_player$Tackle[17],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="SIME VRSALJKO"){
      valueBox(
        team_player$Tackle[18],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="MARCELO BROZOVIC"){
      valueBox(
        team_player$Tackle[19],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="DOMAGOJ VIDA"){
      valueBox(
        team_player$Tackle[20],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="VEDRAN CORLUKA"){
      valueBox(
        team_player$Tackle[21],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else if(input$player=="DANIJEL SUBASIC"){
      valueBox(
        team_player$Tackle[22],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }else{
      valueBox(
        team_player$Tackle[11],
        "搶斷",
        icon=icon("ban"),
        color="orange"
      )
    }
  })



output$touch=renderValueBox({
    if(input$player=="HARRY KANE"){
      valueBox(
        team_player$Touch[1],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="KIERAN TRIPPIER"){
      valueBox(
        team_player$Touch[2],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="RAHEEM STERLING"){
      valueBox(
        team_player$Touch[3],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="DELE ALLI"){
      valueBox(
        team_player$Touch[4],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="JESSE LINGARD"){
      valueBox(
        team_player$Touch[5],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="JORDAN HENDERSON"){
      valueBox(
        team_player$Touch[6],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="HARRY MAGUIRE"){
      valueBox(
        team_player$Touch[7],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="JOHN STONES"){
      valueBox(
        team_player$Touch[8],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="KYLE WALKER"){
      valueBox(
        team_player$Touch[9],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="ASHLEY YOUNG"){
      valueBox(
        team_player$Touch[10],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="MARIO MANDZUKIC"){
      valueBox(
        team_player$Touch[12],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="MATEO KOVACIC"){
      valueBox(
        team_player$Touch[13],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="LUKA MODRIC"){
      valueBox(
        team_player$Touch[14],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="IVAN RAKITIC"){
      valueBox(
        team_player$Touch[15],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="IVAN PERISIC"){
      valueBox(
        team_player$Touch[16],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="NIKOLA KALINIC"){
      valueBox(
        team_player$Touch[17],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="SIME VRSALJKO"){
      valueBox(
        team_player$Touch[18],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="MARCELO BROZOVIC"){
      valueBox(
        team_player$Touch[19],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="DOMAGOJ VIDA"){
      valueBox(
        team_player$Touch[20],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="VEDRAN CORLUKA"){
      valueBox(
        team_player$Touch[21],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else if(input$player=="DANIJEL SUBASIC"){
      valueBox(
        team_player$Touch[22],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }else{
      valueBox(
        team_player$Touch[11],
        "觸球",
        icon=icon("hand-paper-o"),
        color="maroon"
      )
    }
  })



  playerInput=reactive({
    switch(input$player,
           "HARRY KANE"=position_data[,1:2],
           "RAHEEM STERLING"=position_data[,3:4],
           "DELE ALLI"=position_data[,5:6],
           "JORDAN PICKFORD"=position_data[,7:8],
           "MARIO MANDZUKIC"=position_data[,9:10],
           "IVAN PERISIC"=position_data[,11:12],
           "IVAN RAKITIC"=position_data[,13:14],
           "DANIJEL SUBASIC"=position_data[,15:16],
           "JESSE LINGARD"=position_data[,17:18],
           "KIERAN TRIPPIER"=position_data[,19:20],
           "JOHN STONES"=position_data[,21:22],
           "HARRY MAGUIRE"=position_data[,23:24],
           "KYLE WALKER"=position_data[,25:26],
           "JORDAN HENDERSON"=position_data[,27:28],
           "ASHLEY YOUNG"=position_data[,29:30]
           )
  })
  output$heatmap=renderPlot(
    Hmap(playerInput()),height=400,width=711,bg="transparent"
  )
  
  observeEvent(
    input$team,
    updateSelectInput(session,"player","Player",choices=as.character(team_player$Player[team_player$Team==input$team]
    )))
    
    
  
}

shinyApp(ui,server)
