#include "pch.h"
#include "framework.h"
#include "Directory.hpp"
#include <filesystem>
#include <string>
#include <algorithm>

using namespace std;

namespace Puma {
namespace File
{
	namespace Directory
	{
		// Get current working directory - normalized to forward slashes
		Types::String GetCurrentDirectory() noexcept
		{
			error_code ec;
			const filesystem::path path = filesystem::current_path(ec);
			// On error, return empty string
			if (ec)
			{
				return Types::String();
			}

			// Normalize to forward slashes - Puma does not support backslashes
			string normalizedPath = path.string();
			replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');
			// Return as Types::String
			return Types::String(normalizedPath.data(), normalizedPath.size());
		}

		// Set current working directory - normalized to forward slashes
		static bool SetCurrentDirectory(const Types::String& path) noexcept
		{
			const uint32_t pathSize = path.Size();
			// Empty path is invalid
			if (pathSize == 0)
			{
				return false;
			}
			// All modern day OSes support forward slashes, so normalize for portable code
			string normalizedPath(path.First(), path.Last());
			replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');
			// Set current path
			error_code ec;
			filesystem::current_path(normalizedPath, ec);
			// Return success status
			return !ec;
		}
	} // namespace Directory
} // namespace File
} // namespace Puma