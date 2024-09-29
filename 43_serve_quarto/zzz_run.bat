@echo off
start mongoose.exe
timeout /t 1 /nobreak >nul
start chrome http://localhost:8000/test_ojs.html