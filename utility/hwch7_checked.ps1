# 디렉토리 A와 B의 경로 설정
$directoryA = "D:\work\lecture\Lecture_EOPL\hw\hw_ch7"
$directoryB = "D:\work\lecture\Lecture_EOPL_Exercise\ch7\app\checkedlang"
$directoryC = "D:\work\lecture\Lecture_EOPL_Exercise\ch7"

# 디렉토리 A 내의 모든 TypeCheck.hss 파일 찾기
$files = Get-ChildItem -Path $directoryA -Filter TypeCheck.hs -Recurse

# TypeCheck.hs 파일의 개수 출력
Write-Output "Total: $($files.Count)"

foreach ($file in $files) {
    # 각 파일의 경로 출력
    Write-Output "파일 경로: $($file.FullName)"
    
    # 파일을 디렉토리 B로 복사
    Copy-Item -Path $file.FullName -Destination $directoryB
    
    # 현재 디렉토리를 B로 변경
    Push-Location -Path $directoryC
    
    # stack runhaskell Set9aTest.hs 실행
    stack clean
    stack test
    
    # 원래 디렉토리로 돌아가기
    Pop-Location
}
