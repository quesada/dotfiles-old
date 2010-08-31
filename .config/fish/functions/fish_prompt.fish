function fish_prompt --description 'Write out the prompt'
  set -l prompt (color_print (command_status_color) '♪'                  )
  set -l hostname   (color_print (set_color blue)      (hostname))
  set -l git    (color_print (set_color red)        (__git_ps1))
  set -l pwd    (color_print (set_color green)      (pwd))

	echo (printf '%s%s' $hostname $pwd $git) "$prompt "
end
