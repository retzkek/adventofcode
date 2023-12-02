defmodule AOC do
  defp load_token() do
    File.read!(Path.join([System.user_home(), ".config", "aocd", "token"]))
  end

  defp fetch_input(year, day, path) do
    :inets.start
    :ssl.start

    {:ok, {{_version, 200, _reason_phrase}, _headers, body}} =
      :httpc.request(
        :get,
        {~c"https://adventofcode.com/#{year}/day/#{day}/input",
         [
           {~c"Cookie", "session=" <> load_token()}
         ]},
        [],
        []
      )

    :ssl.stop
    :inets.stop

    File.write!(path, body)
  end

  def input(year, day) do
    path = Path.join([System.user_home(), ".config", "aocd", "#{year}", "#{day}"])
    if !File.exists?(path) do
      File.mkdir(path)
    end
    file = Path.join([path, "input.txt"])
    if !File.exists?(file) do
      fetch_input(year, day, file)
    end
    File.read!(file)
  end

  def input(year, day, :lines) do
    String.split(input(year,day))
  end
end
