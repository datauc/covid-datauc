function idleTimer() {
    var t = setTimeout(logout, 160000);
    window.onmousemove = resetTimer; // catches mouse movements
    window.onmousedown = resetTimer; // catches mouse movements
    window.onclick = resetTimer;     // catches mouse clicks
    window.onscroll = resetTimer;    // catches scrolling
    window.onkeypress = resetTimer;  //catches keyboard actions

    function logout() {
      window.close();  //close the window
    }

    function resetTimer() {
      clearTimeout(t);
      t = setTimeout(logout, 160000);  // time is in milliseconds (1000 is 1 second)
    }
  }
  idleTimer();