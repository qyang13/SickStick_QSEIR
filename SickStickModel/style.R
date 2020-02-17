body_style <- tags$style(type="text/css", "

           .title-box {
           position: absolute;
           text-align: center;
           top: 50%;
           left: 50%;
           transform:translate(-50%, -50%);
           }
           @media (max-width: 590px) {
           .title-box {
           position: absolute;
           text-align: center;
           top: 10%;
           left: 10%;
           transform:translate(-5%, -5%);
           }
           }
           @media (max-width: 767px) {
           .primary-title {
           font-size: 1.1em;
           }
           .primary-subtitle {
           font-size: 1em;
           }
           }
           .main-header .logo {
           height: 50px;
           }
           .main-header .navbar {
           height: 20px;
           }

           @media (max-width: 5000px) {
           .main-header {
           padding: 0 0;
           position: relative;
           }
           .main-header .logo{
           width: 100%;
           float: none;
           }
           .main-header .navbar {
           float: none;
           
           margin: 0;
           }
           .main-header .navbar-custom-menu {
           float: right;
           }
           }
           .main-sidebar {
           position: absolute;
           }
           .left-side, .main-sidebar {
           padding-top: 150px;
           }

           /* Color settings below!!!! */
           /* logo */
           .skin-black .main-header .logo {
           background-color: white;
           }
           
           /* logo when hovered */
           .skin-black .main-header .logo:hover {
           background-color: white;
           }
           
           /* navbar (rest of the header) */
           .skin-black .main-header .navbar {
           background-color: #516377;
           }

           .skin-black .box {
           background-color: #232937;
           }
          .box-title {
            color: #31AE84;
          }
           
           /* main sidebar */
           .skin-black .main-sidebar {
           background-color: #516377;
           }
           
           /* toggle button when hovered  */
           .skin-black .main-header .navbar .sidebar-toggle:hover{
           background-color: #ADD7FF;
           }

            .control-label {
            color: white;
            }

           #run {
            background-color: #232937;
            color: #DF4577;
            display: block;
            margin-left: auto;
                         margin-right: auto;
                         width: 80%;
           }

           .skin-black .wrapper {
           background-color: #516377;
                         }
           .content{
               background-color: #232937;
           }
           .content-wrapper{
               background-color: #232937;
           }
           .navbar-default{
             background-color: #232937;
           }
          .navbar-nav{

          }
          .tab-content{
              color: white;
          }
          table {
            color: black;
          }

        
          
"
)