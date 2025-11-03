
# App formatting


appThemeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # General CSS
    tags$style(HTML("
      /* overall light grey page background */
      body, .content-wrapper {
        background-color: #f2f4f5 !important;
      }
      
   /* app font */    
    * {
      font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif !important;
    }
   
    /* make sure drop down menu items appear above other elements */ 
    .dropdown-menu {
    z-index: 2000 !important;
    }
  
  /* title bar background colour */
    .navbar.navbar-default {
    background-color: #ffffff !important;
    border-color: #e7e7e7 !important;
  }

    /* spacing between navbar and page content */
    body > div > .container-fluid:nth-of-type(1) {
        margin: 0 auto;
        padding-top: 65px;
    }

    /* align menu to the right */
    body > div > nav .nav.navbar-nav {
        float: right !important;
    }

    /* keep logo on the left */
    .navbar-header {
        float: left !important;
    }

    /* layout for title+logo */
    #logo {
        display: flex;
        align-items: center;
        gap: 10px;
    }

    /* vertical divider */
    .divider {
        border-left: 1px solid #004b84;
        height: 30px;
        margin-right: 10px;
    }

    /* tab menu spacing */
    .nav-tabs > li {
        float: left;
        margin-bottom: -1px;
        padding-right: 100px;
    }
    ")),
    
    
    
    
    
    
    ### --- Collapse menu when screen is narrow ------------
    tags$style(HTML("
  /* collapse navbar earlier */
  @media (max-width: 1150px) {

    /* show hamburger toggle button */
    .navbar-toggle {
      display: block !important;
    }

    /* prevent nav items staying on one line */
    .navbar-nav {
      float: none !important;
    }

    /* stacked menu items */
    .navbar-nav > li {
      float: none !important;
    }

    /* title area centered on collapse */
    .navbar-header {
      float: none !important;
    }

    /* ensure menu actually collapses/expands */
    .navbar-collapse.collapse {
      display: none !important;
    }
    .navbar-collapse.in {
      display: block !important;
    }
  }
")),
    
    # Close dropdown when menu item selected
    tags$script(HTML("
    $(document).on('click', '.navbar-collapse.in a', function() {
      $('.navbar-collapse').collapse('hide');
    });
  ")),
    
    ### Make this dropdown background blue -----------
    tags$style(HTML("
  @media (max-width: 1150px) {
  
    /* Hamburger icon background colour */
    .navbar-toggle {
      background-color: transparent !important;
      border-color: #cccccc !important;
    }

    /* Hamburger bars colour */
    .navbar-toggle .icon-bar {
      background-color: #888 !important;
    }
    
      /* Hover and focus state */
    .navbar-default .navbar-toggle:hover,
    .navbar-default .navbar-toggle:focus {
        background-color: #e7e7e7 !important; 
        border-color: #cccccc !important; 
    }
    

    /* force background before/during/after collapse */
    .navbar-default .navbar-collapse,
    .navbar-default .navbar-collapse.collapsing,
    .navbar-default .navbar-collapse.in {
      background-color: #004b84 !important;
    }

    /* menu link colors */
    .navbar-default .navbar-nav > li > a {
      color: white !important;
    }

    .navbar-default .navbar-nav > li > a:hover {
      background-color: #033a67 !important;
      color: #ffffff !important;
    }
    
     /* selected menu item formatting */
    .navbar-default .navbar-nav > .active > a {
      background-color: #033a67 !important;
      color: white !important;
    }

    /* hover state for active tab formatting */
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #033a67 !important;
      color: white !important;
    }

  }
")), 
    
    
    
    
    ### Formatting menu item text (both versions) ----------
    # Non-collapsed menu (horizontal)
    tags$style(HTML("
    /* Normal menu items */
    .navbar-default .navbar-nav > li > a {
      color: #3f5262 !important;  /* default text color */
      font-size: 16px;
      font-weight: 500;
      text-decoration: none;       /* no underline */
      letter-spacing:0.5px
    }
    
    /* Hover state: underline and color change */
    .navbar-default .navbar-nav > li > a:hover {
      text-decoration: underline !important;
      color: #0d5088 !important;
      background-color: #e7e7e7 !important;
    }
    
    /* Active (selected) tab: underline and color change */
    .navbar-default .navbar-nav:not(.in) > .active > a {
      text-decoration: underline !important;
      color: #0d5088 !important;
      background-color: #e7e7e7 !important;
    }
    
    /* Active tab on hover */
    .navbar-default .navbar-nav:not(.in) > .active > a:hover {
      text-decoration: underline !important;
      color: #0d5088 !important;
      background-color: #e7e7e7 !important;
    }
    ")),
    
    ## Collapsed menu (hamburger)
    tags$style(HTML("
    /* Collapsed menu items inside hamburger */
    .navbar-collapse.in .navbar-nav > li > a {
      color: white !important;  /* keep white text */
      background-color: #004b84 !important;  /* dark blue background */
      text-decoration: none !important;
    }
    
    /* Hover state in hamburger */
    .navbar-collapse.in .navbar-nav > li > a:hover {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
    /* Active item in hamburger */
    .navbar-collapse.in .navbar-nav > .active > a {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
    /* Active item hover in hamburger */
    .navbar-collapse.in .navbar-nav > .active > a:hover {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
    
    # Make sure formatting is correct during collapsing animation:
        /* Collapsed menu items (hamburger) */
    .navbar-collapse.in .navbar-nav > li > a,
    .navbar-collapse.collapsing .navbar-nav > li > a {
      color: white !important;
      background-color: #004b84 !important; /* dark blue background */
      text-decoration: none !important;
    }

    /* Hover state in hamburger */
    .navbar-collapse.in .navbar-nav > li > a:hover,
    .navbar-collapse.collapsing .navbar-nav > li > a:hover {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
    /* Active item in hamburger */
    .navbar-collapse.in .navbar-nav > .active > a,
    .navbar-collapse.collapsing .navbar-nav > .active > a {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
    /* Active item hover in hamburger */
    .navbar-collapse.in .navbar-nav > .active > a:hover,
    .navbar-collapse.collapsing .navbar-nav > .active > a:hover {
      background-color: #033a67 !important;
      color: white !important;
      text-decoration: underline !important;
    }
    
      /* Force text white during collapse animation */
    .navbar-collapse.collapsing .navbar-nav > li > a {
        color: white !important;
    }"
    ))
  )
}

