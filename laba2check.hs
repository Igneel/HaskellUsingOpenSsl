-- Комментарии начинаются с этих символов
-- импорт необходимых библиотек
import System.Directory
import System.IO
import System.Cmd
import System.FilePath (splitExtension)
import System.Exit
import Data.Time (getCurrentTime)


-- функция verify получается два параметра - имя файла с открытым ключом и имя файла, подпись которого нужно проверить
-- имя файла с подписью совпадается с именем проверяемого файла, за исключением расширения - .sign
-- функция system выполняет стороннюю программу и возвращает код выхода
-- функция unwords склеивает массив строк в одну сроку, через пробелы
-- ключевое слово where определяет локальные функции
-- функция splitExtension разделяет имя файла на непосредственно имя и его расширение, возвращает кортеж из двух строк
-- функция fst возвращает первый элемент кортежа
-- функция ++ склеивает строки
verify public_key file = system (unwords ["openssl dgst -signature", signature, "-verify", public_key, file])
    where signature=fst (splitExtension file) ++ ".sign"

-- функция sign подписывает файл, работает аналогично функции verify
sign private_key file = system (unwords ["openssl dgst -passin pass:1234 -sign", private_key, "-out", signature, "-md5", file])
 where signature=fst (splitExtension file) ++ ".sign"

-- эта функция генерирует открытый и закрытый ключи
-- нотация do позволяет выполнять последовательные действия
generateKeys = do system "openssl genrsa -passout pass:1234 -out rsa_key.pem -des3"
                  system "openssl rsa -in rsa_key.pem -passin pass:1234  -out pubrsa_key.pem -pubout"

-- обёртка над функцией sign, подписывает список файлов
-- функция mapM является функцией высшего порядка - она принимает в качестве одного из параметров функцию
-- её задача состоит в применении полученной функции к каждому элементу списка
-- также эта функция работает с монадическими значениями
signing files= mapM (sign "rsa_key.pem") files

-- обёртка над функцией verify, аналогична функции signing
veryfying files = mapM (verify "pubrsa_key.pem") files

-- получает на вход код выхода программы, возвращает строку с сообщением об успешности верификации
getString a | a== ExitSuccess = "Verified OK"
            | otherwise = "Verified Failure"

-- собирает строку для записи в лог, формирует её из времени, имени файла и сообщении о результатах верификации
-- функция show преобразует любой тип в строку
logString time (file, state) = unwords [show time, file, state]

-- главная функция (как и в Си выполнение программы начинается с неё, если это exe файл)
-- в интерпретаторе любые функции можно вызывать в любом порядке
-- <- - функция присваивания
main = do
    fils<-readFile "input.txt" -- читаем файл
    let files = words fils -- разбиваем прочитанное на слова
    out<-veryfying files  -- проверяем подписи всех файлов
    time<-getCurrentTime  -- получаем текущее время
    -- функция map аналогична функции mapM, но работает с чистыми значениями.
    let out2=map getString out   -- преобразуем результаты проверки в строки
    -- функция zip соединяет две очереди в очередь кортежей
    -- это лучше рассмотреть на примере
    -- a=[1,2,3]
    -- b=["a","b","c"]
    -- zip a b = [(1,"a"),(2,"b"),(3,"c")]
    let out3=map (logString time) (zip files out2) -- формируем строки для логов
    -- функция unlines склеивает массив строк и добавляет на место соединения символ перехода на новую строку
    appendFile "output.txt" (unlines out3) -- дописываем результаты в файл логов



