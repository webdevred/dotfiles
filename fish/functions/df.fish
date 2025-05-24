function df --description 'alias df=df -x tmpfs -x overlay -x zfs -hT'
 command df -x efivarfs -x tmpfs -x zfs -hT $argv
        
end
