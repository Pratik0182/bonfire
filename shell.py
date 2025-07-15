import bonfire
while True:
	txt = input('Bonfire --> ')
	res, error = bonfire.run('<stdin>', txt)

	if error: print(error.res())
	else: print(res)